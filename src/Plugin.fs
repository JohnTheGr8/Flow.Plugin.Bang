namespace Flow.Plugin.Bang

open Flow.Launcher.Plugin
open System
open System.Collections.Generic
open System.Threading
open IcedTasks

module PluginResult =

    let mutable PluginContext = PluginInitContext()

    let openUrl (url: string) = 
        do PluginContext.API.OpenUrl url
        true

    let changeQuery (bang: string) =
        PluginContext.API.ChangeQuery $"%s{bang} "
        false

    let ofBangSuggestion (search: BangPhraseSuggestion) = 
        Result (
            Title     = search.phrase,
            SubTitle  = "Search " + search.snippet,
            Score     = search.score,
            IcoPath   = "icon.png",
            Action    = (fun _ -> changeQuery search.phrase),
            // autocomplete emulates the ChangeQuery behaviour
            AutoCompleteText = search.phrase + " "
        )

    let ofBangDetails (search: string) (details: BangDetails) =
        Result (
            Title      = details.phrase + " : search " + details.snippet,
            SubTitle   = "Type a search term",
            IcoPath    = "icon.png",
            Score      = details.score,
            Action     = (fun _ -> changeQuery details.phrase),
            // autocomplete emulates the ChangeQuery behaviour
            AutoCompleteText = details.phrase + " ",
            // highlight the bang at the start of the title
            TitleHighlightData = List<int> [ 0 .. search.Length - 1 ]
        )

    let ofBangSearch (result: BangSearchResult) =
        let title = $"Search %s{result.bang.snippet} for '%s{result.search}'"
        let hlStart = title.IndexOf result.search

        Result (
            Title      = title,
            SubTitle   = result.redirect,
            Score      = 10000,
            IcoPath    = "icon.png",
            Action     = (fun _ -> openUrl result.redirect),
            CopyText   = result.redirect,
            AutoCompleteText = result.redirect,
            // highlight the search part of the title
            TitleHighlightData = List<int> [ hlStart .. hlStart + result.search.Length - 1 ]
        )

    let bangUnknown bang = 
        Result (
            Title      = "Unknown bang",
            SubTitle   = $"Bang `%s{bang}` could not be found",
            Score      = 10000,
            IcoPath    = "icon.png",
            AutoCompleteText = bang
        )

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
            |> CancellableTask.map (List.map PluginResult.ofBangSuggestion)

        | BangSearch (bang, "") ->
            // just a bang typed
            Ducky.getBangSuggestions bang
            |> CancellableTask.map (List.map (PluginResult.ofBangDetails bang))

        | BangSearch (bang, siteSearch) ->
            // bang phrase and search
            Ducky.searchWithBang bang siteSearch
            |> CancellableTask.map (function
                | Some result -> List.singleton (PluginResult.ofBangSearch result)
                | None -> List.singleton (PluginResult.bangUnknown bang)
            )

        | _ ->
            CancellableTask.singleton []

    let tryHandleQuery query = 
        cancellableTask {
            try
                return! handleQuery query
            with exn ->
                return List.singleton (PluginResult.apiError exn)
        }

type BangPlugin() =

    interface IAsyncPlugin with
        member __.InitAsync (context: PluginInitContext) =
            task {
                PluginResult.PluginContext <- context
                // run a search to fill our cache
                let! _ = QueryImpl.handleQuery ("!", "") CancellationToken.None

                ()
            }

        member __.QueryAsync(query: Query, token: CancellationToken) =
            task {
                let! results = QueryImpl.tryHandleQuery (query.FirstSearch, query.SecondToEndSearch) token

                return List<Result> results
            }
