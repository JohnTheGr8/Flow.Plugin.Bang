namespace Wox.Plugin.Bang

//--------------------------------------------
//  DuckDuckGo API
//--------------------------------------------

open Newtonsoft.Json
open RestSharp

type AutoCompleteSuggestion =
    { phrase        : string
      score         : int
      snippet       : string
      image         : string }

type BangResult =
    { Redirect      : string }

type BangClient() =
    
    member __.getBangSuggestions bang = async {

        let restClient = RestClient "https://duckduckgo.com/"

        let req = RestRequest ( "ac/", Method.GET )
    
        req.AddParameter ("q", bang) |> ignore
    
        let! resp = 
            restClient.ExecuteAsync req
            |> Async.AwaitTask
    
        return JsonConvert.DeserializeObject<AutoCompleteSuggestion list> resp.Content
    }

    member __.getBangSearchResults search = async {

        let restClient = RestClient "https://api.duckduckgo.com/"

        let req = RestRequest ( "/", Method.GET )
    
        req.AddParameter ("q", search) |> ignore
        req.AddParameter ("format", "json") |> ignore
        req.AddParameter ("no_redirect", 1) |> ignore
    
        let! resp = 
            restClient.ExecuteAsync req
            |> Async.AwaitTask
    
        return JsonConvert.DeserializeObject<BangResult> resp.Content
    }

//--------------------------------------------
//  Plugin
//--------------------------------------------

open Wox.Plugin
open System
open System.Collections.Generic
open System.Diagnostics

type BangPlugin() =

    let client = BangClient()

    let mutable PluginContext = PluginInitContext()

    // cache !bangs and their snippet names
    let cache = Dictionary<string, string> ()

    let openUrl (url:string) = 
        Process.Start url |> ignore
        true

    let changeQuery (bang:string) =
        PluginContext.API.ChangeQuery <| sprintf "%s " bang
        false

    let continueWith f = 
        Async.Catch 
        >> Async.RunSynchronously 
        >> function
        | Choice1Of2 result -> f result
        | Choice2Of2 error -> [ Result ( Title = "Error occured", SubTitle = error.Message, IcoPath = "icon.png" ) ]

    interface IPlugin with
        member this.Init (context:PluginInitContext) = 
            PluginContext <- context

        member this.Query (q:Query) =

            if String.IsNullOrWhiteSpace q.Search ||
               q.Search = "!" ||
               not (q.FirstSearch.StartsWith("!")) then List<Result> [] else

            let search = q.Search.Split ' ' |> Array.toList

            match search with
            | [ b ] -> 
                client.getBangSuggestions b
                |> continueWith ( List.map (fun s -> 
                    cache.[s.phrase] <- s.snippet

                    Result ( Title     = s.phrase,
                             SubTitle  = sprintf "Search %s" s.snippet,
                             Score     = s.score,
                             IcoPath    = "icon.png",
                             Action    = fun _ -> changeQuery s.phrase )))

            | [ b; e ] when String.IsNullOrWhiteSpace e ->

                [ Result ( Title      = sprintf "Search %s" cache.[b],
                           SubTitle   = "Type a search term",
                           IcoPath    = "icon.png",
                           Score      = 10000 ) ]

            | h::tail -> 
                let search = String.concat " " (h :: tail)

                client.getBangSearchResults search
                |> continueWith (fun r ->
                    [ Result ( Title      = sprintf "Search %s for '%s'" cache.[h] q.SecondToEndSearch,
                               SubTitle   = r.Redirect,
                               Score      = 10000,
                               IcoPath    = "icon.png",
                               Action     = fun _ -> openUrl r.Redirect ) ] )

            | [ ] -> []

            |> List<Result>