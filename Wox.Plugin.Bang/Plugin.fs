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
    
        req.AddParameter ("q", bang)
    
        let! resp = 
            restClient.ExecuteTaskAsync req 
            |> Async.AwaitTask
    
        return JsonConvert.DeserializeObject<AutoCompleteSuggestion list> resp.Content
    }

    member __.getBangSearchResults search = async {

        let restClient = RestClient "https://api.duckduckgo.com/"

        let req = RestRequest ( "/", Method.GET )
    
        req.AddParameter ("q", search)
        req.AddParameter ("format", "json")
        req.AddParameter ("no_redirect", 1)
    
        let! resp = 
            restClient.ExecuteTaskAsync req 
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
        | Choice2Of2 error -> [ Result ( Title = "Error occured", SubTitle = error.Message ) ]

    interface IPlugin with
        member this.Init (context:PluginInitContext) = 
            PluginContext <- context

        member this.Query (q:Query) =
            
            if not (q.FirstSearch.StartsWith("!")) then List<Result> [] else

            let search = q.Search.Split ' ' |> Array.toList

            match search with
            | [ b ] -> 
                client.getBangSuggestions b
                |> continueWith ( List.map (fun s -> 
                    Result ( Title     = s.phrase,
                             SubTitle  = s.snippet,
                             Action    = fun _ -> changeQuery s.phrase )))

            | [ b; e ] when String.IsNullOrWhiteSpace e ->
                [ Result ( Title      = "Type Search Term",
                           SubTitle   = "" ) ]

            | h::tail -> 
                let search = String.concat " " (h :: tail)

                client.getBangSearchResults search
                |> continueWith (fun r ->
                    [ Result ( Title      = "Search",
                               SubTitle   = r.Redirect,
                               Action     = fun _ -> openUrl r.Redirect ) ] )

            | [ ] -> []

            |> List<Result>