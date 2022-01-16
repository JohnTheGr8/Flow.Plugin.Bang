module Tests

open Expecto
open Expecto.Flip
open Flow.Plugin.Bang

let allTests =
    testList "all tests" [

        testList "DuckDuckGoApi.getBangSuggestions" [

            testAsync "just the bang" {
                let! results = DuckDuckGoApi.getBangSuggestions "!"

                results
                    |> Expect.isNonEmpty "there should be some results"
            }

            testAsync "github bang phrases" {
                let! results = DuckDuckGoApi.getBangSuggestions "!gh"

                (results.Length, 10)
                    |> Expect.isGreaterThan "should have at least 10 suggestions"

                results
                    |> List.tryFind (fun r -> r.phrase = "!gh" && r.snippet = "GitHub")
                    |> Expect.isSome "there should be a !gh result"

                results
                    |> List.tryFind (fun r -> r.phrase = "!ghcode" && r.snippet = "Github (code search)")
                    |> Expect.isSome "there should be a !ghcode result"
            }

            testAsync "google bang phrases" {
                let! results = DuckDuckGoApi.getBangSuggestions "!g"

                results
                    |> List.tryFind (fun r -> r.phrase = "!g" && r.snippet = "Google")
                    |> Expect.isSome "there should be a !g result"

                results
                    |> List.tryFind (fun r -> r.phrase = "!gi" && r.snippet = "Google Images")
                    |> Expect.isSome "there should be a !gi result"
            }
        ]

        testAsync "DuckDuckGoApi.getBangSearchResults" {
            let! result = DuckDuckGoApi.getBangSearchResults "!gh" "Wox Plugin"

            result.Redirect |> Expect.equal "redirect URL should match" "https://github.com/search?utf8=%E2%9C%93&q=Wox%20Plugin"
        }

        testList "QueryImpl.handleQuery" [

            testAsync "no query terms" {
                let! results = QueryImpl.handleQuery ("", "")

                results |> Expect.isEmpty "should be empty"
            }

            testAsync "just a bang phrase" {
                let! results = QueryImpl.handleQuery ("!imdb", "")
                let actual = List.tryHead results

                actual                  |> Expect.isSome "results should not be empty"
                actual.Value.Title      |> Expect.equal "result title should equal"    "!imdb : search IMDB"
                actual.Value.SubTitle   |> Expect.equal "result subtitle should equal" "Type a search term"
            }

            testAsync "just a bang phrase, again" {
                let! results = QueryImpl.handleQuery ("!tw", "")
                let actual = List.tryHead results

                actual                  |> Expect.isSome "results should not be empty"
                actual.Value.Title      |> Expect.equal "result subtitle should equal" "!tw : search Twitter"
                actual.Value.SubTitle   |> Expect.equal "result subtitle should equal" "Type a search term"
            }

            testAsync "bang phrase and search" {
                let! results = QueryImpl.handleQuery ("!gh", "Wox")
                let actual = List.tryExactlyOne results

                actual                  |> Expect.isSome "there should be exactly one result"
                actual.Value.SubTitle   |> Expect.equal "redirect URL should match" "https://github.com/search?utf8=%E2%9C%93&q=Wox"
            }

            testAsync "recently used bangs" {

                let bangs =
                    List.replicate 50 "!yt"
                  @ List.replicate 30 "!steam"
                  @ List.replicate 10 "!wiki"

                for bang in bangs do
                    do! QueryImpl.handleQuery (bang, "") |> Async.Ignore

                let! results = QueryImpl.handleQuery ("!", "")

                (results.Length, 3)
                    |> Expect.isGreaterThanOrEqual "there should be at least 3 results"

                results.[0].Title |> Expect.equal "first item should be"  "!yt"
                results.[1].Title |> Expect.equal "second item should be" "!steam"
                results.[2].Title |> Expect.equal "third item should be"  "!wiki"
            }
        ]

        testList "cached methods" [

            testAsync "getBangSuggestions performance" {

                let notCached () =
                    DuckDuckGoApi.getBangSuggestions "!nf"
                    |> Async.RunSynchronously
                    |> List.map (fun x -> { x with score = 0 })

                let cached () =
                    Ducky.getBangSuggestions "!nf"
                    |> Async.RunSynchronously
                    |> List.map (fun x -> { x with score = 0 })

                (notCached, cached) ||> Expect.isFasterThan "cached getBangSuggestions is faster"
            }

            testAsync "getBangDetails performance" {

                let notCached () =
                    DuckDuckGoApi.getBangDetails "!maps"
                    |> Async.RunSynchronously
                    |> Option.map (fun x -> { x with score = 0 })

                let cached () =
                    Ducky.getBangDetails "!maps"
                    |> Async.RunSynchronously
                    |> Option.map (fun x -> { x with score = 0 })

                (notCached, cached) ||> Expect.isFasterThan "cached getBangDetails is faster"
            }
        ]
    ]
