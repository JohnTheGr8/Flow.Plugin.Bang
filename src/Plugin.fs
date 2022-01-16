namespace Flow.Plugin.Bang

open Flow.Launcher.Plugin
open System
open System.Collections.Generic
open System.Threading
open System.Threading.Tasks

module PluginResult =

    let mutable PluginContext = PluginInitContext()

    let openUrl (url: string) = 
        do PluginContext.API.OpenUrl url
        true

    let changeQuery (bang: string) =
        PluginContext.API.ChangeQuery $"%s{bang} "
        false

    let ofBangSuggestion (search: BangPhraseSuggestion) = 
        Result ( Title     = search.phrase,
                 SubTitle  = sprintf "Search %s" search.snippet,
                 Score     = search.score,
                 IcoPath   = "icon.png",
                 Action    = fun _ -> changeQuery search.phrase )

    let ofBangDetails (details: BangDetails) = 
        Result ( Title      = sprintf "%s : search %s" details.phrase details.snippet,
                 SubTitle   = "Type a search term",
                 IcoPath    = "icon.png",
                 Score      = details.score )

    let ofBangSearch (result: BangSearchResult) =
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

    let (|BangSearch|_|) (first: string, rest: string) =
        if first.StartsWith "!" && not (first.Contains " ") then
            if String.IsNullOrWhiteSpace rest then
                Some (first, "")
            else
                Some (first, rest)
        else
            None

    let handleQuery = function
        | BangSearch ("!", "") ->
            // just the bang symbol was typed
            Ducky.getBangSuggestionsOrDefault ()
            |> AsyncList.map PluginResult.ofBangSuggestion

        | BangSearch (bang, "") ->
            // just a bang typed
            Ducky.getBangSuggestions bang
            |> AsyncList.map PluginResult.ofBangDetails

        | BangSearch (bang, siteSearch) ->
            // bang phrase and search
            Ducky.searchWithBang bang siteSearch
            |> Async.foldOption PluginResult.ofBangSearch (PluginResult.bangUnknown bang)
            |> AsyncList.singleton

        | _ ->
            async { return [] }

type BangPlugin() =

    interface IAsyncPlugin with
        member this.InitAsync (context: PluginInitContext) : Task =
            PluginResult.PluginContext <- context

            QueryImpl.handleQuery ("!", "")
            |> Async.StartAsTask :> Task

        member __.QueryAsync(query: Query, token: CancellationToken) =
            let asyncQuery =
                QueryImpl.handleQuery (query.FirstSearch, query.SecondToEndSearch)
                |> Async.Catch
                |> Async.map (function
                    | Choice1Of2 results -> List<Result> results
                    | Choice2Of2 error   -> List<Result> [ PluginResult.apiError error ])

            Async.StartImmediateAsTask(asyncQuery, token)
