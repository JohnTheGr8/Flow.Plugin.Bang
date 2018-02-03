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

    let getBangSuggestions bang = async {
    
        let restClient = RestClient "https://duckduckgo.com/"

        let req = RestRequest ( "ac/", Method.GET )
    
        req.AddParameter ("q", bang)
    
        let! resp = 
            restClient.ExecuteTaskAsync req 
            |> Async.AwaitTask
    
        return JsonConvert.DeserializeObject<AutoCompleteSuggestion list> resp.Content
    }

    let getBangSearchResults search = async {

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
open System.Collections.Generic

type BangPlugin() =

    let mutable PluginContext = PluginInitContext()

    interface IPlugin with
        member this.Init (context:PluginInitContext) = 
            PluginContext <- context

        member this.Query (q:Query) =
        
            List<Result> [ Result ( Title = "", SubTitle = "" ) ]