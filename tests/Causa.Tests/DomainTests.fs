module Causa.Tests.DomainTests

open System
open Expecto
open Causa.Domain
open Causa.Domain.Json
open Causa.Domain.Engine
open Causa.Domain.FileSet

let private ids = ref 0

let ctx : Context =
    { NewId = fun () -> ids.Value <- ids.Value + 1; sprintf "00000000-0000-4000-8000-%012d" ids.Value
      Now = fun () -> "2026-10-29T23:00:00.000Z"
      Actor = { ActorId = "test"; DisplayName = None } }

let investigationId = "11111111-1111-4111-8111-111111111111"

let decideAll (cmds: Command list) =
    cmds
    |> List.fold
        (fun (state, events) cmd ->
            match decide ctx investigationId state cmd with
            | Ok(env, next) -> Some next, events @ [ env ]
            | Error e -> failwithf "refused: %s" e)
        (None, [])

let cmd t fields = { EventType = t; Payload = JObject fields }

let start = cmd "InvestigationStarted" [ "title", JString "Lamp failed"; "originalProblem", JString "The lamp is dark." ]

let hypothesisId = "22222222-2222-4222-8222-222222222222"

let basic =
    [ start
      cmd "HypothesisProposed" [ "hypothesisId", JString hypothesisId; "statement", JString "The bulb burned out"; "mechanism", JNull ] ]

let fileSet (cmds: Command list) =
    let state, events = decideAll cmds
    build "" state.Value events "2026-10-29T23:00:00.000Z"

let tests =
    testList "domain" [
        testList "canonical JSON (RFC 8785)" [
            test "sorts keys by UTF-16 code units and removes whitespace" {
                let json = parse """{ "b": 1, "a": [true, null, "x"], "A": {"z": "\u00e9", "y": "\n"} }""" |> Result.defaultWith failwith
                Expect.equal (canonical json) "{\"A\":{\"y\":\"\\n\",\"z\":\"é\"},\"a\":[true,null,\"x\"],\"b\":1}" "canonical form"
            }
            test "refuses duplicate keys" { Expect.isError (parse """{"a":1,"a":2}""") "duplicate keys fail closed" }
            test "escapes control characters in lowercase hex" { Expect.equal (canonical (JString "\u0001")) "\"\\u0001\"" "control escape" }
        ]
        testList "registry" [
            test "unknown event types fail closed" {
                Expect.isError (Registry.validatePayload "SomethingHappened" (JObject [])) "unknown type"
            }
            test "unknown payload fields fail closed" {
                Expect.isError (Registry.validatePayload "InvestigationRenamed" (JObject [ "title", JString "x"; "extra", JString "y" ])) "extra field"
            }
            test "enumerations are exhaustive" {
                let p = JObject [ "unknownId", JString hypothesisId; "question", JString "q"; "materiality", JString "huge" ]
                Expect.isError (Registry.validatePayload "UnknownIdentified" p) "value outside enumeration"
            }
        ]
        testList "transitions (STATE-TRANSITION-TABLES §2)" [
            let changeTo target =
                cmd "HypothesisStateChanged" [ "hypothesisId", JString hypothesisId; "to", JString target; "reason", JString "because"; "evidenceIds", JArray [] ]
            for target in Registry.Enums.hypothesis do
                test (sprintf "proposed -> %s follows the table" target) {
                    let state, _ = decideAll basic
                    let legal = Set.contains ("proposed", target) transitions.["hypothesis"]
                    Expect.equal (Result.isOk (decide ctx investigationId state (changeTo target))) legal "decide agrees with the table"
                }
            test "a resolution needs a recorded conclusion" {
                let state, _ = decideAll (basic @ [ cmd "InvestigationActivated" [ "reason", JString "go" ] ])
                let forged = cmd "InvestigationResolved" [ "reason", JString "done"; "conclusionId", JString "33333333-3333-4333-8333-333333333333" ]
                Expect.isError (decide ctx investigationId state forged) "forged resolution refused"
            }
            test "legal targets come from the engine" {
                let state, _ = decideAll basic
                let h = state.Value.Entities.[hypothesisId]
                let targets = legalTargets ctx state.Value h |> List.map snd |> Set.ofList
                Expect.equal targets (set [ "plausible"; "falsified"; "superseded" ]) "only legal moves offered"
            }
        ]
        testList "file sets (causa-data v1.0.0 §4-§8)" [
            test "a built file set verifies and replays" {
                let files = fileSet basic
                let v = verify investigationId files |> Result.defaultWith failwith
                Expect.equal v.Investigation.Version 2L "replayed version"
                Expect.equal v.Investigation.Entities.Count 1 "one hypothesis"
            }
            test "appending keeps the earlier event bytes" {
                let files = fileSet basic
                let v = verify investigationId files |> Result.defaultWith failwith
                let env, next =
                    decide ctx investigationId (Some v.Investigation) (cmd "ObservationRecorded" [ "observationId", JString "44444444-4444-4444-8444-444444444444"; "original", JString "dark"; "observedAt", JNull ])
                    |> Result.defaultWith failwith
                let files2 = build files.Events next [ env ] "2026-10-29T23:01:00.000Z"
                Expect.stringStarts files2.Events files.Events "history is append-only"
                Expect.isOk (verify investigationId files2) "the appended file set verifies"
            }
            let files = fileSet basic
            let lines = files.Events.TrimEnd('\n').Split('\n')
            let negative name (mutate: Files -> Files) =
                test (sprintf "refuses %s" name) { Expect.isError (verify investigationId (mutate files)) name }
            negative "an altered prior event" (fun f -> { f with Events = f.Events.Replace("The bulb burned out", "The fuse blew") })
            negative "a duplicate version" (fun f -> { f with Events = f.Events + lines.[1] + "\n" })
            negative "a missing version" (fun f -> { f with Events = lines.[0] + "\n" })
            negative "a manifest hash mismatch" (fun f -> { f with Manifest = f.Manifest.Replace("\"eventCount\":2", "\"eventCount\":3") })
            negative "the wrong investigation id" (fun f -> { f with Events = f.Events.Replace(investigationId, "55555555-5555-4555-8555-555555555555") })
            negative "an unknown event type" (fun f -> { f with Events = f.Events.Replace("HypothesisProposed", "HypothesisGuessed") })
            negative "CRLF line endings" (fun f -> { f with Events = f.Events.Replace("\n", "\r\n") })
            negative "a snapshot that disagrees with history" (fun f -> { f with Snapshot = f.Snapshot.Replace("Lamp failed", "Lamp fine") })
            negative "an unsupported format version" (fun f -> { f with Manifest = f.Manifest.Replace("\"formatVersion\":\"1.0.0\"", "\"formatVersion\":\"2.0.0\"") })
        ]
    ]
