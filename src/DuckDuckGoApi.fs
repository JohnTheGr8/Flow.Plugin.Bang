namespace Flow.Plugin.Bang

type BangPhraseSuggestion =
    { phrase        : string
      score         : int
      snippet       : string
      image         : string }

type BangResult =
    { Redirect      : string }

type BangDetails = BangPhraseSuggestion

type BangSearchResult =
    { bang      : BangDetails
      search    : string
      redirect  : string }

module DuckDuckGoApi =

    open Newtonsoft.Json
    open RestSharp
    open IcedTasks

    let private httpClient = 
        new RestClient(
            RestClientOptions(
                ThrowOnAnyError = true
            )
        )

    let private getResponse<'response> request = cancellableTask {
        let! token =
            CancellableTask.getCancellationToken()

        let! response =
            httpClient.ExecuteAsync(request, token)

        do token.ThrowIfCancellationRequested()

        return JsonConvert.DeserializeObject<'response> response.Content
    }

    let getBangSuggestions (bang: string) =
        let request = 
            RestRequest( "https://duckduckgo.com/ac/", Method.Get )
                .AddParameter("q", bang)

        getResponse<BangPhraseSuggestion list> request

    let getBangSearchResults bang siteSearch =
        let request =
            RestRequest( "https://api.duckduckgo.com/", Method.Get )
                .AddParameter("q", $"%s{bang} %s{siteSearch}")
                .AddParameter("format", "json")
                .AddParameter("no_redirect", 1)

        getResponse<BangResult> request

    let getBangDetails bang = cancellableTask {
        let! suggestions =
            getBangSuggestions bang

        return suggestions |> List.tryFind (fun b -> b.phrase = bang)
    }

module Ducky =
    open System.Collections.Concurrent
    open IcedTasks

    /// we cache every found bang search term and its search results
    let private bangSuggestionsCache =
        ConcurrentDictionary<string, BangPhraseSuggestion list> ()

    /// we cache every found bang phrase and its details
    let private knownBangsCache =
        ConcurrentDictionary<string, BangDetails> ()

    let clearCache () = 
        do bangSuggestionsCache.Clear()
        do knownBangsCache.Clear()

    /// DuckDuckGoApi.getBangSuggestions with caching
    let getBangSuggestions bang = cancellableTask {
        match bangSuggestionsCache.TryGetValue bang with
        | true, res ->
            // if a full bang was typed and it exists in our knownBangsCache, bump its score
            match res |> List.tryFind (fun x -> x.phrase = bang) with
            | Some suggestion -> 
                do knownBangsCache.AddOrUpdate(bang, suggestion, fun _ b -> { b with score = b.score + 1 }) |> ignore
            | None -> 
                ()

            return res

        | false, _ ->
            let! results = DuckDuckGoApi.getBangSuggestions bang
            // add all suggestions to the cache
            bangSuggestionsCache.[bang] <- results

            for result in results do
                // also add every result to the details cache
                knownBangsCache.TryAdd (result.phrase, { result with score = 0 }) |> ignore

            return results
        }

    /// DuckDuckGoApi.getBangDetails with caching
    let getBangDetails bang = cancellableTask {
        match knownBangsCache.TryGetValue bang with
        | true, res ->
            let result = { res with score = res.score + 1 }
            knownBangsCache.[bang] <- result
            return Some result
        | false, _ ->
            match! DuckDuckGoApi.getBangDetails bang with
            | Some result ->
                let result = { result with score = 0 }
                // add retrieved details to the cache
                knownBangsCache.[result.phrase] <- result
                return Some result
            | None ->
                return None
        }

    /// combine getBangDetails and getBangSuggestions,
    let searchWithBang bang siteSearch = cancellableTask {
        match! getBangDetails bang with
        | Some details ->
            let! result = DuckDuckGoApi.getBangSearchResults bang siteSearch

            return Some { bang = details
                          search = siteSearch
                          redirect = result.Redirect }
        | None ->
            return None
    }

    /// if we have known !bangs stored in cache, return them sorted by most used
    /// if not, return the default !bang suggestions from the DDG API
    let getBangSuggestionsOrDefault () = 
        if knownBangsCache.IsEmpty then
            getBangSuggestions "!"
        else
            knownBangsCache.Values
                |> Seq.sortByDescending (fun b -> b.score)
                |> List.ofSeq
                |> CancellableTask.singleton
