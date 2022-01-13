namespace Wox.Plugin.Bang

[<RequireQualifiedAccess>]
module Async =

    let map mapper fCompute = async {
        let! x = fCompute
        return mapper x 
    }

    let foldOption folder noneValue fCompute = async {
        match! fCompute with
        | Some value -> return folder value
        | None -> return noneValue
    }

[<RequireQualifiedAccess>]
module AsyncList =

    let map mapping fComputeList = Async.map (List.map mapping) fComputeList

    let singleton fComputeList = Async.map List.singleton fComputeList