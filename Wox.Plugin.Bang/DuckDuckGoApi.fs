namespace Wox.Plugin.Bang

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

    let private getResponse<'response> (client: RestClient) request = async {
        let! response =
            client.ExecuteAsync request |> Async.AwaitTask

        return JsonConvert.DeserializeObject<'response> response.Content
    }

    let getBangSuggestions =
        let restClient = RestClient "https://duckduckgo.com/"

        fun (bang: string) ->
            let req = RestRequest ( "ac/", Method.GET )
            req.AddParameter ("q", bang) |> ignore

            getResponse<BangPhraseSuggestion list> restClient req

    let getBangSearchResults =
        let restClient = RestClient "https://api.duckduckgo.com/"

        fun bang siteSearch ->
            let search =
                sprintf "%s %s" bang siteSearch

            let request =
                RestRequest( "/", Method.GET )
                    .AddParameter("q", search)
                    .AddParameter("format", "json")
                    .AddParameter("no_redirect", 1)

            getResponse<BangResult> restClient request

    let getBangDetails bang = async {
        let! suggestions =
            getBangSuggestions bang

        return suggestions |> List.tryFind (fun b -> b.phrase = bang)
    }

module Ducky =
    open System.Collections.Concurrent

    /// we cache every found bang search term and its search results
    let private bangSuggestionsCache =
        ConcurrentDictionary<string, BangPhraseSuggestion list> ()

    /// we cache every found bang phrase and its details
    let private knownBangsCache =
        ConcurrentDictionary<string, BangDetails> ()

    /// DuckDuckGoApi.getBangSuggestions with caching
    let getBangSuggestions bang = async {
        match bangSuggestionsCache.TryGetValue bang with
        | true, res ->
            return res
        | false, _ ->
            let! results = DuckDuckGoApi.getBangSuggestions bang
            // add all suggestions to the cache
            bangSuggestionsCache.[bang] <- results

            for result in results do
                // also add every result to the details cache
                knownBangsCache.TryAdd (result.phrase, result) |> ignore

            return results
        }

    /// DuckDuckGoApi.getBangDetails with caching
    let getBangDetails bang = async {
        match knownBangsCache.TryGetValue bang with
        | true, res ->
            return Some res
        | false, _ ->
            match! DuckDuckGoApi.getBangDetails bang with
            | Some result ->
                // add retrieved details to the cache
                knownBangsCache.[result.phrase] <- result
                return Some result
            | None ->
                return None
        }

    /// combine getBangDetails and getBangSuggestions, 
    let searchWithBang bang siteSearch = async {
        match! getBangDetails bang with
        | Some details ->
            let! result = DuckDuckGoApi.getBangSearchResults bang siteSearch

            return Some { bang = details
                          search = siteSearch
                          redirect = result.Redirect }
        | None ->
            return None
    }
