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

let start = cmd "InvestigationStarted" [ "title", JString "Lamp failed"; "observationId", JString "99999999-9999-4999-8999-999999999999"; "original", JString "The lamp is dark." ]

let hypothesisId = "22222222-2222-4222-8222-222222222222"

let basic =
    [ start
      cmd "HypothesisRecorded" [ "hypothesisId", JString hypothesisId; "statement", JString "The bulb burned out"; "mechanism", JNull ] ]

let fileSet (cmds: Command list) =
    let state, events = decideAll cmds
    build "" state.Value events

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
                cmd "HypothesisTransitioned" [ "hypothesisId", JString hypothesisId; "to", JString target; "reason", JString "because"; "evidenceIds", JArray [] ]
            for target in Registry.Enums.hypothesis do
                test (sprintf "proposed -> %s follows the table" target) {
                    let state, _ = decideAll basic
                    let legal = hypothesisCommands |> List.exists (fun (_, ps) -> List.contains ("proposed", target) ps)
                    Expect.equal (Result.isOk (decide ctx investigationId state (changeTo target))) legal "decide agrees with the table"
                }
            test "a lifecycle change must follow the command table" {
                let state, _ = decideAll basic
                let skip = cmd "LifecycleChanged" [ "to", JString "resolved"; "reason", JNull ]
                Expect.isError (decide ctx investigationId state skip) "draft cannot be resolved"
                let activate = cmd "LifecycleChanged" [ "to", JString "active"; "reason", JNull ]
                let env, next = decide ctx investigationId state activate |> Result.defaultWith failwith
                Expect.equal next.Lifecycle "active" "activated"
                Expect.equal (field "command" env.Payload) (Some(JString "activateInvestigation")) "the command is recorded"
            }
            test "InvestigationStarted records the first observation" {
                let state, _ = decideAll [ start ]
                Expect.equal (state.Value.Entities |> Map.toList |> List.map (fun (_, e) -> e.Kind)) [ "observation" ] "one observation"
            }
            test "legal targets come from the engine" {
                let state, _ = decideAll basic
                let h = state.Value.Entities.[hypothesisId]
                let targets = legalTargets ctx state.Value h |> List.map snd |> Set.ofList
                Expect.equal targets (set [ "plausible"; "falsified"; "superseded" ]) "only legal moves offered"
            }
        ]
        testList "derived next steps (UI-032)" [
            test "a hypothesis without evidence is a next step that names it" {
                let state, _ = decideAll basic
                let steps = Derived.nextSteps state.Value
                Expect.equal (steps |> List.map (fun s -> s.EntityId)) [ hypothesisId ] "one step, linked to its record"
            }
            test "relating evidence removes the step" {
                let evidenceId = "66666666-6666-4666-8666-666666666666"
                let state, _ =
                    decideAll (
                        basic
                        @ [ cmd "EvidenceAdmitted" [
                                "evidenceId", JString evidenceId
                                "contentKind", JString "text"
                                "content", JString "dark filament"
                                "category", JString "directObservation"
                                "source", JString "eyes"
                                "capturedAt", JNull ]
                            cmd "EvidenceRelated" [
                                "relationshipId", JString "77777777-7777-4777-8777-777777777777"
                                "evidenceId", JString evidenceId
                                "hypothesisId", JString hypothesisId
                                "kind", JString "supports"
                                "rationale", JNull ] ])
                Expect.isEmpty (Derived.nextSteps state.Value) "no invented work"
            }
        ]
        testList "file sets (causa-data v1.0.0 §4-§8)" [
            test "a built file set verifies and replays" {
                let files = fileSet basic
                let v = verify investigationId files |> Result.defaultWith failwith
                Expect.equal v.Investigation.Version 2L "replayed version"
                Expect.equal v.Investigation.Entities.Count 2 "the first observation and one hypothesis"
            }
            test "appending keeps the earlier event bytes" {
                let files = fileSet basic
                let v = verify investigationId files |> Result.defaultWith failwith
                let env, next =
                    decide ctx investigationId (Some v.Investigation) (cmd "ObservationRecorded" [ "observationId", JString "44444444-4444-4444-8444-444444444444"; "original", JString "dark"; "observedAt", JNull ])
                    |> Result.defaultWith failwith
                let files2 = build files.Events next [ env ]
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
            negative "an unknown event type" (fun f -> { f with Events = f.Events.Replace("HypothesisRecorded", "HypothesisGuessed") })
            negative "CRLF line endings" (fun f -> { f with Events = f.Events.Replace("\n", "\r\n") })
            negative "a snapshot that disagrees with history" (fun f -> { f with Snapshot = f.Snapshot.Replace("Lamp failed", "Lamp fine") })
            negative "a snapshot whose entity state disagrees with history" (fun f -> { f with Snapshot = f.Snapshot.Replace("\"state\":\"proposed\"", "\"state\":\"supported\"") })
            negative "an unsupported format version" (fun f -> { f with Manifest = f.Manifest.Replace("\"formatVersion\":\"1.0.0\"", "\"formatVersion\":\"2.0.0\"") })
        ]
    ]
