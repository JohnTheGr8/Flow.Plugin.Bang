namespace Wox.Plugin.Bang

open Wox.Plugin

module PluginResult =

    let ofBangSuggestion changeQuery (search: BangPhraseSuggestion) = 
        Result ( Title     = search.phrase,
                 SubTitle  = sprintf "Search %s" search.snippet,
                 Score     = search.score,
                 IcoPath   = "icon.png",
                 Action    = fun _ -> changeQuery search.phrase )

    let ofBangDetails (details: BangDetails) = 
        Result ( Title      = sprintf "Search %s" details.snippet,
                 SubTitle   = "Type a search term",
                 IcoPath    = "icon.png",
                 Score      = 10000 )

    let ofBangSearch openUrl (result: BangSearchResult) =
        Result ( Title      = sprintf "Search %s for '%s'" result.bang.snippet result.search,
                 SubTitle   = result.redirect,
                 Score      = 10000,
                 IcoPath    = "icon.png",
                 Action     = fun _ -> openUrl result.redirect )

    let bangUnknown bang = 
        Result ( Title      = "Unknown bang",
                 SubTitle   = sprintf "Bang `%s` could not be found" bang,
                 IcoPath    = "icon.png",
                 Score      = 10000 )

    let apiError (exn: exn) = 
        Result ( Title = "Error occured", SubTitle = exn.Message, IcoPath = "icon.png" )

module QueryImpl = 

    let (|BangSearch|_|) (search: string) = 
        if search.Contains " " then None else
        if search.StartsWith "!" then Some search else
        None

    let handleQuery changeQuery openUrl = function
        | [ BangSearch bang ] ->
            // just the bang symbol was typed
            Ducky.getBangSuggestions bang
            |> AsyncList.map (PluginResult.ofBangSuggestion changeQuery)
        
        | [ BangSearch bang; "" ] ->
            // bang phrase and empty search
            Ducky.getBangDetails bang
            |> Async.foldOption PluginResult.ofBangDetails (PluginResult.bangUnknown bang)
            |> AsyncList.singleton

        | BangSearch bang :: siteSearch ->
            // bang phrase and search
            Ducky.searchWithBang bang (String.concat " " siteSearch)
            |> Async.foldOption (PluginResult.ofBangSearch openUrl) (PluginResult.bangUnknown bang)
            |> AsyncList.singleton

        | _ ->
            async { return [] }

open System.Collections.Generic
open System.Diagnostics

type BangPlugin() =

    let mutable PluginContext = PluginInitContext()

    let openUrl (url:string) = 
        Process.Start url |> ignore
        true

    let changeQuery (bang:string) =
        PluginContext.API.ChangeQuery <| sprintf "%s " bang
        false

    interface IPlugin with
        member this.Init (context:PluginInitContext) = 
            PluginContext <- context

        member this.Query (query: Query) =
            List.ofArray query.Terms
            |> QueryImpl.handleQuery changeQuery openUrl
            |> Async.Catch
            |> Async.RunSynchronously
            |> function
                | Choice1Of2 results -> results
                | Choice2Of2 error -> [ PluginResult.apiError error ]
            |> List<Result>
