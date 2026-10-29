module Causa.Tests.Program

open Expecto

[<EntryPoint>]
let main argv =
    let live = PatTests.liveTests ()
    let all =
        [ DomainTests.tests; SaveStateTests.tests; PatTests.tests ] @ (live |> Option.toList)
    if live.IsNone then printfn "LIVE: BLOCKED (CAUSA_WRITE_TOKEN is not set); fake-mode results are not live verification."
    runTestsWithCLIArgs [] argv (testList "causa" all)
