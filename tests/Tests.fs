module Tests

open Expecto
open Expecto.Flip
open Flow.Launcher.Plugin
open Flow.Plugin.Bang
open System.Threading

let allTests =
    testList "all tests" [

        testList "DuckDuckGoApi.getBangSuggestions" [

            testTask "just the bang" {
                let! results = DuckDuckGoApi.getBangSuggestions "!" CancellationToken.None

                results
                    |> Expect.isNonEmpty "there should be some results"
            }

            testTask "github bang phrases" {
                let! (results: list<_>) = DuckDuckGoApi.getBangSuggestions "!gh" CancellationToken.None

                (results.Length, 10)
                    |> Expect.isGreaterThan "should have at least 10 suggestions"

                results
                    |> List.tryFind (fun r -> r.phrase = "!gh" && r.snippet = "GitHub")
                    |> Expect.isSome "there should be a !gh result"

                results
                    |> List.tryFind (fun r -> r.phrase = "!ghcode" && r.snippet = "Github (code search)")
                    |> Expect.isSome "there should be a !ghcode result"
            }

            testTask "google bang phrases" {
                let! results = DuckDuckGoApi.getBangSuggestions "!g" CancellationToken.None

                results
                    |> List.tryFind (fun r -> r.phrase = "!g" && r.snippet = "Google")
                    |> Expect.isSome "there should be a !g result"

                results
                    |> List.tryFind (fun r -> r.phrase = "!gi" && r.snippet = "Google Images")
                    |> Expect.isSome "there should be a !gi result"
            }
        ]

        testTask "DuckDuckGoApi.getBangSearchResults" {
            let! result = DuckDuckGoApi.getBangSearchResults "!gh" "Wox Plugin" CancellationToken.None

            result.Redirect |> Expect.equal "redirect URL should match" "https://github.com/search?utf8=%E2%9C%93&q=Wox%20Plugin"
        }

        testList "QueryImpl.handleQuery" [

            testTask "no query terms" {
                let! (results: list<Result>) = QueryImpl.handleQuery ("", "") CancellationToken.None

                results |> Expect.isEmpty "should be empty"
            }

            testTask "just a bang phrase" {
                let! (results: list<Result>) = QueryImpl.handleQuery ("!imdb", "") CancellationToken.None
                let actual = List.tryHead results

                actual                  |> Expect.isSome "results should not be empty"
                actual.Value.Title      |> Expect.equal "result title should equal"    "!imdb : search IMDB"
                actual.Value.SubTitle   |> Expect.equal "result subtitle should equal" "Type a search term"
            }

            testTask "just a bang phrase, again" {
                let! (results: list<Result>) = QueryImpl.handleQuery ("!tw", "") CancellationToken.None
                let actual = List.tryHead results

                actual                  |> Expect.isSome "results should not be empty"
                actual.Value.Title      |> Expect.equal "result subtitle should equal" "!tw : search Twitter"
                actual.Value.SubTitle   |> Expect.equal "result subtitle should equal" "Type a search term"
            }

            testTask "bang phrase and search" {
                let! (results: list<Result>) = QueryImpl.handleQuery ("!gh", "Wox") CancellationToken.None
                let actual = List.tryExactlyOne results

                actual                  |> Expect.isSome "there should be exactly one result"
                actual.Value.SubTitle   |> Expect.equal "redirect URL should match" "https://github.com/search?utf8=%E2%9C%93&q=Wox"
            }

            testTask "recently used bangs" {

                let bangs =
                    List.replicate 50 "!yt"
                  @ List.replicate 30 "!steam"
                  @ List.replicate 10 "!lb"

                for bang in bangs do
                    let! _ = QueryImpl.handleQuery (bang, "") CancellationToken.None
                    ()

                let! (results: list<Result>) = QueryImpl.handleQuery ("!", "") CancellationToken.None

                (results.Length, 3)
                    |> Expect.isGreaterThanOrEqual "there should be at least 3 results"

                results.[0].Title |> Expect.equal "first item should be"  "!yt"
                results.[1].Title |> Expect.equal "second item should be" "!steam"
                results.[2].Title |> Expect.equal "third item should be"  "!lb"
            }
        ]

        testList "cached methods" [

            testTask "getBangSuggestions performance" {

                let notCached () =
                    DuckDuckGoApi.getBangSuggestions "!nf" CancellationToken.None
                    |> Async.AwaitTask
                    |> Async.RunSynchronously
                    |> List.map (fun x -> { x with score = 0 })

                let cached () =
                    Ducky.getBangSuggestions "!nf" CancellationToken.None
                    |> Async.AwaitTask
                    |> Async.RunSynchronously
                    |> List.map (fun x -> { x with score = 0 })

                (notCached, cached) ||> Expect.isFasterThan "cached getBangSuggestions is faster"
            }

            testTask "getBangDetails performance" {

                let notCached () =
                    DuckDuckGoApi.getBangDetails "!maps" CancellationToken.None
                    |> Async.AwaitTask
                    |> Async.RunSynchronously
                    |> Option.map (fun x -> { x with score = 0 })

                let cached () =
                    Ducky.getBangDetails "!maps" CancellationToken.None
                    |> Async.AwaitTask
                    |> Async.RunSynchronously
                    |> Option.map (fun x -> { x with score = 0 })

                (notCached, cached) ||> Expect.isFasterThan "cached getBangDetails is faster"
            }
        ]
    ]
