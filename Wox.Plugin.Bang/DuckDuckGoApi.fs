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
