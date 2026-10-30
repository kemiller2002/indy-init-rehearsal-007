/// PAT-12 (PERSIST-051): every save state against every input of the save-state machine.
module Causa.Tests.SaveStateTests

open Expecto
open Causa.Domain.FileSet
open Causa.Persistence

let private files = { Manifest = "m"; Events = "e"; Snapshot = "s" }
let private confirmation = Confirmation.ofReadBack "inv" "abc" files files "2026-10-29T23:00:00.000Z" |> Result.defaultWith failwith

let states =
    [ Saved confirmation; Unsaved; Saving; Offline; Conflict; UnknownOutcome; SaveFailed RateLimited ]

let inputs =
    [ Edited true; Edited false; SaveRequested true; SaveRequested false; NotSent; MutationSent; StaleUnrelated; StaleConflict
      Confirmed(confirmation, false, true); Confirmed(confirmation, true, true); Confirmed(confirmation, true, false)
      OutcomeLost; Failed CredentialRefused; NothingWritten; ConnectivityLost; ConnectivityRestored; Discarded confirmation
      Reapplied(confirmation, true, true); Reapplied(confirmation, false, true); Opened confirmation ]

/// The expected machine, written from PERSIST-031 independently of the implementation.
/// None means the input is illegal in that state and must fail closed.
let expected (state: SaveState) (input: SaveInput) : string option =
    let s = SaveState.name state
    match s, input with
    | ("Saved" | "Unsaved" | "Offline"), Edited true -> Some "Unsaved"
    | ("Saved" | "Unsaved" | "Offline"), Edited false -> Some "Offline"
    | "Save failed", Edited _ -> Some "Save failed"
    | ("Saving" | "Unknown commit outcome"), Edited _ -> Some s
    | "Saved", (ConnectivityLost | ConnectivityRestored) -> Some "Saved"
    | ("Unsaved" | "Save failed" | "Offline"), ConnectivityLost -> Some "Offline"
    | ("Unsaved" | "Offline"), ConnectivityRestored -> Some "Unsaved"
    | "Save failed", ConnectivityRestored -> Some "Save failed"
    | ("Unsaved" | "Offline" | "Save failed"), SaveRequested true -> Some "Saving"
    | ("Unsaved" | "Offline" | "Save failed"), SaveRequested false -> Some "Offline"
    | "Saving", NotSent -> Some "Offline"
    | ("Saving" | "Unknown commit outcome" | "Conflict"), (ConnectivityLost | ConnectivityRestored) -> Some s
    | "Saving", (MutationSent | StaleUnrelated) -> Some "Saving"
    | ("Saved" | "Unsaved"), Opened _ -> Some "Saved"
    | ("Saving" | "Unknown commit outcome"), StaleConflict -> Some "Conflict"
    | ("Saving" | "Unknown commit outcome"), Confirmed(_, false, _) -> Some "Saved"
    | ("Saving" | "Unknown commit outcome"), Confirmed(_, true, true) -> Some "Unsaved"
    | ("Saving" | "Unknown commit outcome"), Confirmed(_, true, false) -> Some "Offline"
    | ("Saving" | "Unknown commit outcome"), OutcomeLost -> Some "Unknown commit outcome"
    | ("Saving" | "Unknown commit outcome"), Failed _ -> Some "Save failed"
    | "Unknown commit outcome", (NothingWritten | StaleUnrelated) -> Some "Saving"
    | "Conflict", Discarded _ -> Some "Saved"
    | "Conflict", Reapplied(_, true, true) -> Some "Unsaved"
    | "Conflict", Reapplied(_, false, _) -> Some "Saved"
    | _ -> None

let tests =
    testList "PAT-12 save-state machine" [
        let label =
            function
            | Edited o | SaveRequested o -> sprintf "(online=%b)" o
            | Confirmed(_, r, o) -> sprintf "(unsavedRemain=%b, online=%b)" r o
            | Reapplied(_, a, o) -> sprintf "(anyAccepted=%b, online=%b)" a o
            | _ -> ""
        for state in states do
            for input in inputs do
                test (sprintf "%s + %s%s" (SaveState.name state) (SaveState.inputName input) (label input)) {
                    match expected state input, SaveState.step state input with
                    | Some want, Ok got -> Expect.equal (SaveState.name got) want "reaches the state PERSIST-031 defines"
                    | None, Error fault -> Expect.equal fault.From (SaveState.name state) "fails closed with a typed fault"
                    | Some want, Error f -> failtestf "expected %s, got fault %s" want f.Reason
                    | None, Ok got -> failtestf "illegal input accepted, reached %s" (SaveState.name got)
                }
        test "Saved is reached only through an input that carries a GitHub confirmation" {
            for state in states do
                for input in inputs do
                    match SaveState.step state input with
                    | Ok(Saved _) when SaveState.name state <> "Saved" ->
                        match input with
                        | Confirmed _ | Discarded _ | Reapplied _ | Opened _ -> ()
                        | other -> failtestf "%s reached Saved from %s" (SaveState.inputName other) (SaveState.name state)
                    | _ -> ()
        }
        test "a read-back with different bytes yields no confirmation" {
            Expect.isError (Confirmation.ofReadBack "inv" "abc" files { files with Events = "other" } "t") "mismatch refused"
        }
    ]
