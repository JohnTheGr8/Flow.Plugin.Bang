module Tests

open Expecto
open Expecto.Flip
open Wox.Plugin.Bang

let checkQuery : _ -> Async<Wox.Plugin.Result list> =
    let doNothing = fun _ -> false
    QueryImpl.handleQuery doNothing doNothing

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
                let! results = checkQuery []

                results |> Expect.isEmpty "should be empty"
            }

            testAsync "just a bang phrase" {
                let! results = checkQuery [ "!imdb" ]
                let actual = List.tryHead results

                actual                  |> Expect.isSome "results should not be empty"
                actual.Value.Title      |> Expect.equal "result title should equal"    "!imdb"
                actual.Value.SubTitle   |> Expect.equal "result subtitle should equal" "Search IMDB"
            }

            testAsync "bang phrase and space" {
                let! results = checkQuery [ "!tw"; "" ]
                let actual = List.tryExactlyOne results

                actual              |> Expect.isSome "there should be exactly one result"
                actual.Value.Title  |> Expect.equal "result subtitle should equal" "Search Twitter"
            }

            testAsync "bang phrase and search" {
                let! results = checkQuery [ "!gh"; "Wox" ]
                let actual = List.tryExactlyOne results

                actual                  |> Expect.isSome "there should be exactly one result"
                actual.Value.SubTitle   |> Expect.equal "redirect URL should match" "https://github.com/search?utf8=%E2%9C%93&q=Wox"
            }
        ]
    ]
