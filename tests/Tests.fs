module Tests

open Expecto
open Expecto.Flip
open Wox.Plugin.Bang

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
    ]
