namespace Wox.Plugin.Bang

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