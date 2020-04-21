module Wox.Plugin.Bang.Tests

open Expecto

[<EntryPoint>]
let main argv =
    Tests.runTestsWithArgs defaultConfig argv Tests.allTests
