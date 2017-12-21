open System
open System.Configuration
open Tweetinvi
open Tweetinvi.Models
open Tweetinvi.Parameters
open System.Text.RegularExpressions
open Newtonsoft.Json.Linq
open System.Net
open System.Text
open System.IO
open Microsoft.FSharp.Reflection

type ToneEmotionScores =
    { anger:float32; disgust:float32; fear:float32; joy:float32; sadness:float32 }
    with static member Zero = { anger=0.f; disgust=0.f; fear=0.f; joy=0.f; sadness=0.f }
         static member (+) (toneA, toneB) =
            { anger = (toneA.anger + toneB.anger) / 2.f
              disgust = (toneA.disgust + toneB.disgust) / 2.f
              fear = (toneA.fear + toneB.fear) / 2.f
              joy = (toneA.joy + toneB.joy) / 2.f
              sadness = (toneA.sadness + toneB.sadness) / 2.f }

let toString (un:'a) =
    match FSharpValue.GetUnionFields(un, typeof<'a>) with
    | case, _ -> case.Name

type Emotions =
    | Anger
    | Fear
    | Disgust
    | Joy
    | Sadness
    member this.toString = toString this

type SantaList =
    | Nice
    | Naughty
    member this.toString = toString this

let [<Literal>] maxSizeSentence = 30720
let regx_tweethandle = Regex("@\w+", RegexOptions.Compiled ||| RegexOptions.IgnoreCase)
let regx_hash = Regex("#\w+", RegexOptions.Compiled ||| RegexOptions.IgnoreCase)
let regx_url = Regex("http[^\s]+", RegexOptions.Compiled ||| RegexOptions.IgnoreCase)

[<EntryPoint>]
let main argv =

    let consumerKey = ConfigurationManager.AppSettings.["ConsumerKey"]
    let consumerSecret = ConfigurationManager.AppSettings.["ConsumerSecret"]
    let accessToken = ConfigurationManager.AppSettings.["AccessToken"]
    let accessTokenSecret = ConfigurationManager.AppSettings.["AccessTokenSecret"]
    let usernameWatson = ConfigurationManager.AppSettings.["usernameWatson"]
    let passwordWatson = ConfigurationManager.AppSettings.["passwordWatson"]
    let baseWatsonURL = "https://gateway.watsonplatform.net/tone-analyzer/api/v3/tone?version=2016-05-19&sentences=false"

    let naughtyOrNiceSelector (tones:ToneEmotionScores) =
        match tones with
        | {anger=anger;disgust=disgust;joy=joy} when anger + disgust >= joy -> Naughty
        | _ -> Nice

    do Auth.SetCredentials(new TwitterCredentials(consumerKey, consumerSecret, accessToken, accessTokenSecret))

    let getTweetHistory (batch:int) (userId:string)  =
        let tweetsToFetch = DateTime.Now
        let rec fetchTweets (tweets:ITweet list) =
            let tweetsCount = tweets |> List.sortByDescending(fun t -> t.CreatedAt) |> List.last
            if tweetsCount.CreatedAt >= (tweetsToFetch.AddMonths(-11)) then
                let idOfOldestTweet = tweets |> List.map(fun t -> t.Id) |> List.min
                // MaxId ensures that we only get tweets that have been posted
                let timelineRequestParameters = UserTimelineParameters(MaxId = int64(idOfOldestTweet - 1L),MaximumNumberOfTweetsToRetrieve = batch)
                let lastTweets = Timeline.GetUserTimeline(userId, timelineRequestParameters) |> Seq.toList
                fetchTweets (lastTweets @ tweets)
            else tweets
        let lastTweets = Timeline.GetUserTimeline(userId, batch) |> Seq.toList
        if (lastTweets |> List.sortByDescending(fun t -> t.CreatedAt) |> List.last).CreatedAt >= (tweetsToFetch.AddMonths(-11)) then
            fetchTweets lastTweets
        else
            lastTweets

    let tweetCleanUp (tweets:ITweet list) =
        tweets
        |> List.map(fun t -> t.Text)
        |> List.map(fun t -> regx_tweethandle.Replace(t, ""))
        |> List.map(fun t -> regx_hash.Replace(t, ""))
        |> List.map(fun t -> regx_url.Replace(t, ""))
        |> List.map(fun t -> t.Replace("RT", "").Replace("\"", "\\\""))
        |> List.map (System.Web.HttpUtility.JavaScriptStringEncode)

    let tweetToAnalyzeMerger tweets =
        List.foldBack(fun (tweet:string) acc ->
                let tweets = tweet.Trim().Split([|Environment.NewLine;"\n"|], StringSplitOptions.RemoveEmptyEntries)
                let tweets = tweets |> Array.filter(String.IsNullOrWhiteSpace >> not)
                if tweets.Length = 0 then acc
                else (String.Join(". ", tweets)::acc)) tweets []

    let tweetPartitioner (tweetsToAnalyze:string list) =
        let rec partitionTweets (tweetsToAnalyze:string list) acc =
            match tweetsToAnalyze with
            | [] -> acc
            | tweets -> let tweetChunk, tweetsRemaining = partitionSubTweets tweets []
                        partitionTweets tweetsRemaining (tweetChunk :: acc)
        and partitionSubTweets (tweetsToAnalyze:string list) (acc:string list) =
            match tweetsToAnalyze with
            | head::tail when head.Length + (acc |> List.sumBy(fun s -> s.Length)) <= maxSizeSentence -> partitionSubTweets tail (head::acc)
            | tweets -> (String.Join(". ", acc), tweets)
        partitionTweets tweetsToAnalyze []

    let toneScoresEvaluator (emotionTone:Emotions) (tones:JEnumerable<JToken>) =
        tones
        |> Seq.find(fun tone -> tone.["tone_name"].ToString() = emotionTone.toString)
        |> fun score -> score.["score"] |> float32

    let emotionAnalyzer (tweetChunks:string list) =
            tweetChunks
            |> Seq.map(fun tweetChunk -> async {
                let data = sprintf "{\"text\": \"%s\"}" tweetChunk
                let request = WebRequest.Create(baseWatsonURL)
                let auth = sprintf "%s:%s" usernameWatson passwordWatson
                let auth64 = Convert.ToBase64String(Encoding.ASCII.GetBytes(auth))
                let credentials = sprintf "Basic %s" auth64
                request.Headers.[HttpRequestHeader.Authorization] <- credentials
                request.Method <- "POST"
                request.ContentType <- "application/json"
                let byteArray = Encoding.UTF8.GetBytes(data)
                request.ContentLength <- int64 byteArray.Length
                use! dataStream = request.GetRequestStreamAsync() |> Async.AwaitTask
                do! dataStream.AsyncWrite(byteArray, 0, byteArray.Length)
                let! response = request.AsyncGetResponse()
                use responseStream = response.GetResponseStream()
                use reader = new StreamReader(responseStream)
                let! responseFromServer = reader.ReadToEndAsync() |> Async.AwaitTask
                let resultAnalisys = JObject.Parse(responseFromServer).ToString()
                let docTone = JObject.Parse(resultAnalisys)

                let categories = docTone.["document_tone"].["tone_categories"] :?> JArray
                let emotion_tones = categories.Children() |> Seq.find(fun ch -> ch.["category_id"].ToString() = "emotion_tone")
                let tonesJ = emotion_tones.["tones"].Children()
                return { anger = tonesJ |> toneScoresEvaluator Anger
                         fear = tonesJ |> toneScoresEvaluator Fear
                         disgust = tonesJ |> toneScoresEvaluator Disgust
                         joy = tonesJ |> toneScoresEvaluator Joy
                         sadness = tonesJ |> toneScoresEvaluator Sadness }})
            |> Async.Parallel
            |> Async.RunSynchronously
            |> Array.reduce(+)

    let naughtyOrNiceAnalisys =
       getTweetHistory 1000 >> tweetCleanUp >> tweetToAnalyzeMerger >> tweetPartitioner >> emotionAnalyzer >> naughtyOrNiceSelector
       >> function
          | Nice -> printfn "Congratulations!! It looks like you will be having a very Merry Christmas this year!"
          | Naughty -> printfn "Santa's elves report that you have been naughty this year, its not too late to start behaving!"


    naughtyOrNiceAnalisys "trikace"

    //naughtyOrNiceAnalisys "dsyme"

    Console.ReadLine() |> ignore

    0