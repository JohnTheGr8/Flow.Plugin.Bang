namespace Wox.Plugin.Bang

open Wox.Plugin
open System
open System.Collections.Generic
open System.Diagnostics

type BangPlugin() =

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
                DuckDuckGoApi.getBangSuggestions b
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

            | bang :: siteSearch ->

                DuckDuckGoApi.getBangSearchResults bang (String.concat " " siteSearch)
                |> continueWith (fun r ->
                    [ Result ( Title      = sprintf "Search %s for '%s'" cache.[bang] q.SecondToEndSearch,
                               SubTitle   = r.Redirect,
                               Score      = 10000,
                               IcoPath    = "icon.png",
                               Action     = fun _ -> openUrl r.Redirect ) ] )

            | [ ] -> []

            |> List<Result>