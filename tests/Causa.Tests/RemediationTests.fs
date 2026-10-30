/// Regression tests for the defects the independent review found (evidence/DECISIONS.md KD-021).
/// Each names the finding it guards; each fails against the code before its fix.
module Causa.Tests.RemediationTests

open System
open System.Text.RegularExpressions
open Expecto
open Causa.Domain.Json
open Causa.Domain.FileSet
open Causa.Persistence
open Causa.Persistence.GitHub
open Causa.App
open Causa.Fake
open Causa.Tests.Harness

// ---- SaveProcess driven directly with chosen GitHub answers ----

let private conn = { ApiBase = "https://api.github.com"; Owner = "o"; Name = "n"; Branch = "main"; Workspace = "workspaces/t"; Token = "x" }
let private written = { Manifest = "m\n"; Events = "e\n"; Snapshot = "s\n" }
let private baseCommit = String.replicate 40 "a"
let private landed = String.replicate 40 "b"
let private started () =
    SaveProcess.start conn "id" "workspaces/t/investigations/id/" baseCommit (Some written) written 1 "tab" 0 "headline"

let private committed oid = Answered(200, JObject [ "data", JObject [ "createCommitOnBranch", JObject [ "commit", JObject [ "oid", JString oid ] ] ] ])
let private contents (text: string) =
    Answered(200, JObject [ "type", JString "file"; "encoding", JString "base64"; "content", JString(Convert.ToBase64String(Text.Encoding.UTF8.GetBytes text)) ])
let private fileText (r: Request) =
    if r.Url.Contains "manifest.json" then written.Manifest elif r.Url.Contains "events.ndjson" then written.Events else written.Snapshot

/// One step of the process: what it asked for, and the inputs and waits it produced, until it finishes or waits on the user.
type private Trace = { Requests: Request list; Inputs: SaveInput list; Delays: int list; Outcome: string }

let private drive (answer: Request -> Reply) : Trace =
    let rec go (next: SaveProcess.Next) (t: Trace) (steps: int) =
        if steps > 50 then { t with Outcome = "did not end" }
        else
            match next with
            | SaveProcess.Send(s, r, inputs) -> go (SaveProcess.onReply s (answer r) "now" "nonce") { t with Requests = t.Requests @ [ r ]; Inputs = t.Inputs @ inputs } (steps + 1)
            | SaveProcess.Wait(s, delay, inputs) ->
                let t' = { t with Inputs = t.Inputs @ inputs; Delays = t.Delays @ [ delay ] }
                match SaveProcess.reconcileNow s true "nonce" with
                | Some n -> go n t' (steps + 1)
                | None -> { t' with Outcome = "awaiting the user" }
            | SaveProcess.AwaitUser(_, inputs) -> { t with Inputs = t.Inputs @ inputs; Outcome = "awaiting the user" }
            | SaveProcess.Finish(o, inputs) ->
                let name = match o with SaveProcess.Done c -> "Done " + Confirmation.commit c | SaveProcess.ConflictAt _ -> "Conflict" | SaveProcess.Stopped -> "Stopped"
                { t with Inputs = t.Inputs @ inputs; Outcome = name }
    go (started ()) { Requests = []; Inputs = []; Delays = []; Outcome = "" } 0

let private isMutation (r: Request) = r.Method = "POST"

let saveProcess =
    testList "review remediation: save process" [
        test "M1: a commit is confirmed only by reading its bytes back at the landed commit" {
            let t = drive (fun r -> if isMutation r then committed landed else contents (fileText r))
            Expect.equal t.Outcome ("Done " + landed) "confirmed at the landed commit"
            let reads = t.Requests |> List.filter (isMutation >> not)
            Expect.equal reads.Length 3 "all three files are read back"
            Expect.isTrue (reads |> List.forall (fun r -> r.Url.EndsWith("?ref=" + landed))) "each read is at the landed commit"
        }
        test "M1: a read-back that differs from the bytes written is Save failed, never Saved" {
            let t = drive (fun r -> if isMutation r then committed landed elif r.Url.Contains "events.ndjson" then contents "other\n" else contents (fileText r))
            Expect.equal t.Outcome "Stopped" "not confirmed"
            Expect.contains t.Inputs (Failed ReadBackMismatch) "named as a read-back mismatch"
        }
        test "M2: when every request is lost, the save is never a conflict and never resends blindly" {
            let t = drive (fun _ -> Lost)
            Expect.isFalse (t.Inputs |> List.contains StaleConflict) "a network failure is never a conflict"
            Expect.notEqual t.Outcome "Conflict" "not a conflict"
            Expect.equal (t.Requests |> List.filter isMutation).Length 1 "one mutation; reconciliation decides any resend"
            Expect.equal t.Outcome "awaiting the user" "the outcome stays unknown and waits for the user"
        }
        test "M2: when requests are refused before sending, the save is Offline, not a conflict" {
            let t = drive (fun _ -> Unsent)
            Expect.equal t.Inputs [ MutationSent; NotSent ] "not sent, so Offline"
        }
        test "M7: automatic reconciliation reads are spaced 2, 4 and 8 seconds apart" {
            let t = drive (fun _ -> Lost)
            Expect.equal t.Delays [ 2000; 4000; 8000 ] "the minimum read spacing"
            Expect.equal (t.Requests |> List.filter (isMutation >> not)).Length 3 "at most three automatic reads for one outcome"
        }
        test "F9: an own-write check that finds the base diverged is rewritten history, not a conflict" {
            let stale = Answered(200, JObject [ "errors", JArray [ JObject [ "type", JString "STALE_DATA"; "message", JString "Expected branch to point to x" ] ] ])
            let moved = String.replicate 40 "c"
            let mutable mutationCount = 0
            let answer (r: Request) =
                if isMutation r then
                    mutationCount <- mutationCount + 1
                    if mutationCount = 1 then Lost else stale
                elif r.Url.Contains "/git/ref" then Answered(200, JObject [ "object", JObject [ "sha", JString moved ] ])
                elif r.Url.Contains "/compare/" && r.Url.Contains(baseCommit + "...") && r.Url.Contains moved then
                    Answered(200, JObject [ "status", JString "diverged"; "commits", JArray [] ])
                elif r.Url.Contains "/compare/" then Answered(200, JObject [ "status", JString "identical"; "commits", JArray [] ])
                elif r.Url.Contains("ref=" + moved) then contents "changed by someone\n"
                else contents (fileText r)
            let t = drive answer
            Expect.contains t.Inputs (Failed HistoryRewritten) "diverged history is named"
            Expect.isFalse (t.Inputs |> List.contains StaleConflict) "never reported as a conflict"
        }
    ]

// ---- Confirmation and loading ----

let confirmation =
    testList "review remediation: confirmation" [
        test "F1: a load confirms only bytes that pass full verification" {
            let load, first = Load.start conn "11111111-1111-4111-8111-111111111111" landed
            Expect.stringContains first.Url "manifest.json" "the load reads the manifest first"
            let step1 = Load.onReply load (contents "{}") "now"
            let step2 = match step1 with Load.Next(l, _) -> Load.onReply l (contents "") "now" | other -> other
            let step3 = match step2 with Load.Next(l, _) -> Load.onReply l (contents "{}") "now" | other -> other
            match step3 with
            | Load.Refused _ -> ()
            | _ -> failtest "unverified bytes must not produce a confirmation"
        }
        test "F1: a read that fails is reported, never confirmed" {
            let load, _ = Load.start conn "11111111-1111-4111-8111-111111111111" landed
            match Load.onReply load Lost "now" with
            | Load.NotRead ReadOffline -> ()
            | _ -> failtest "a lost read is not a confirmation"
        }
    ]

// ---- The application, against the stateful fake ----

let private world () =
    let p, _, update = fakeProvider "causa-test" "data" "test-token"
    driver p, workspaceName (), update

let private seeded () =
    let d, ws, update = world ()
    let a = newTab "A"
    connect d a ws
    let id = createInvestigation d a "Remediation"
    save d a id
    Expect.equal (saveState a id) "Saved" "seeded"
    d, ws, update, a, id

/// Runs exactly one queued effect, so a test can act between GitHub's answers.
let private stepOne (d: Driver) =
    match d.Queue with
    | (tab, Limen.Http(cid, r)) :: rest ->
        d.Queue <- rest
        send d tab (Limen.HttpResult(cid, execute d.Provider d.Net tab.Name r))
    | _ :: rest -> d.Queue <- rest
    | [] -> ()

let private markersSent (d: Driver) =
    mutations d
    |> List.collect (fun (_, r) -> [ for m in Regex.Matches(r.Body.Value, markerTrailer + ": ([A-Za-z0-9-]+)") -> m.Groups.[1].Value ])

let application =
    testList "review remediation: application" [
        test "F2: an edit made while a refresh is in flight is kept, never overwritten and shown as Saved" {
            let d, _, _, a, id = seeded ()
            send d a (Limen.Event("refresh", None, None))
            stepOne d // the head read
            observe d a id "edited during refresh"
            settle d [ a ]
            let s = session a id
            Expect.equal (saveState a id) "Unsaved" "the edit is still unsaved"
            Expect.equal s.Unsaved.Length 1 "the edit is kept"
            Expect.isTrue (s.Current.Value.Entities |> Map.exists (fun _ e -> (string e.Fields).Contains "edited during refresh")) "the edit is still in the investigation"
        }
        test "F2: refresh is refused while there are unsaved changes" {
            let d, _, _, a, id = seeded ()
            observe d a id "not yet saved"
            let before = d.Net.Log.Length
            send d a (Limen.Event("refresh", None, None))
            settle d [ a ]
            Expect.equal d.Net.Log.Length before "nothing is read over unsaved work"
            Expect.equal (session a id).Unsaved.Length 1 "the edit is kept"
        }
        test "F15: a refresh whose file read fails leaves the investigation loaded" {
            let d, _, _, a, id = seeded ()
            send d a (Limen.Event("refresh", None, None))
            stepOne d // the head read succeeds
            d.Net.FailReads <- true
            settle d [ a ]
            Expect.equal (session a id).Load App.Ready "still loaded"
            Expect.equal (saveState a id) "Saved" "the confirmed state is unchanged"
        }
        test "F5: write markers stay unique across a disconnect and reconnect" {
            let d, ws, _, a, id = seeded ()
            send d a (Limen.Event("disconnect", None, None))
            connect d a ws
            observe d a id "after reconnecting"
            save d a id
            Expect.equal (saveState a id) "Saved" "saved again"
            let sent = markersSent d
            Expect.equal sent.Length 2 "two mutations"
            Expect.equal (List.distinct sent).Length sent.Length "no marker is sent twice"
        }
        test "F13: a save queued behind another is not sent once the tab knows it is offline" {
            let d, _, _, a, first = seeded ()
            let second = createInvestigation d a "Second"
            observe d a first "pending one"
            let before = (mutations d).Length
            send d a (Limen.Event("save", Some first, None)) // its mutation is queued but not yet answered
            send d a (Limen.Event("save", Some second, None))
            Expect.equal (saveState a second) "Saving" "the second save waits behind the first"
            d.Net.Online <- false
            send d a (Limen.Event("net", None, Some "offline"))
            settle d [ a ]
            Expect.equal ((mutations d).Length - before) 1 "only the mutation already sent; none is sent while known offline"
            Expect.equal (saveState a first) "Offline" "the first save was not sent"
            Expect.equal (saveState a second) "Offline" "the queued save waits as Offline"
        }
    ]

let security =
    testList "review remediation: credential destination" [
        test "F14: the credential is never sent to a plaintext or other host" {
            [ "http://localhost:8080"; "http://127.0.0.1:4300/github"; "https://evil.example"; "http://api.github.com" ]
            |> List.iter (fun api ->
                let t = newTab "S"
                [ "connect.owner", "o"; "connect.repo", "r"; "connect.branch", "main"; "connect.workspace", "workspaces/s/x"; "connect.token", "secret"; "connect.api", api ]
                |> List.iter (fun (k, v) -> t.Model <- fst (App.update t.Env t.Model (Limen.Event("draft:" + k, None, Some v))))
                let m, effects = App.update t.Env t.Model (Limen.Event("connect", None, None))
                Expect.isEmpty effects (sprintf "no request to %s" api)
                Expect.isNone m.Connection (sprintf "%s is refused" api))
        }
    ]

let tests = testList "review remediation" [ saveProcess; confirmation; application; security ]
