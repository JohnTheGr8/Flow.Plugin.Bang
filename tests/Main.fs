module Flow.Plugin.Bang.Tests

open Expecto

[<EntryPoint>]
let main argv =
    Tests.runTestsWithCLIArgs [] argv Tests.allTests
