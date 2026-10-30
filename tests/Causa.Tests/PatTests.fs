/// PAT-02 to PAT-08 and PAT-10: persistence acceptance scenarios, run against the stateful fake and,
/// when CAUSA_WRITE_TOKEN is present, against the live GitHub test repository (PERSIST-025).
module Causa.Tests.PatTests

open System
open Expecto
open Causa.Domain.Json
open Causa.Persistence
open Causa.Persistence.GitHub
open Causa.App
open Causa.Fake
open Causa.Tests.Harness

/// A scenario's world: a driver, the workspace it owns, and fake-only controls (None live).
type World =
    { Driver: Driver
      Workspace: string
      Fake: ((FakeGitHub.State -> FakeGitHub.State) -> unit) option
      Mode: string }

let private prefix (w: World) id = Causa.Domain.FileSet.investigationPrefix w.Workspace id

let private githubEvents (w: World) id =
    w.Driver.Provider.FileAt (w.Driver.Provider.Head()) (prefix w id + "events.ndjson") |> Option.defaultValue ""

let private count (needle: string) (hay: string) =
    let rec go (i: int) n = match hay.IndexOf(needle, i, StringComparison.Ordinal) with -1 -> n | j -> go (j + needle.Length) (n + 1)
    go 0 0

let private expectState (tab: Tab) id want = Expect.equal (saveState tab id) want (sprintf "%s shows %s" tab.Name want)

/// Tab A creates and saves `n` investigations in a fresh workspace.
let private seed (w: World) (n: int) =
    let a = newTab "seed"
    connect w.Driver a w.Workspace
    let ids =
        [ for i in 1..n ->
            let id = createInvestigation w.Driver a (sprintf "Investigation %d" i)
            save w.Driver a id
            expectState a id "Saved"
            id ]
    ids

let private opened (w: World) name =
    let t = newTab name
    connect w.Driver t w.Workspace
    t

let private mutationInputs (d: Driver) =
    mutations d
    |> List.map (fun (_, r) -> parse(r.Body.Value) |> Result.defaultWith failwith |> field "variables" |> Option.bind (field "input") |> Option.get)

let private markers (d: Driver) =
    mutationInputs d |> List.choose (fun i -> i |> field "message" |> Option.bind (field "body") |> function Some(JString b) -> Some b | _ -> None)

let pat02 (w: World) =
    let ids = seed w 3
    let tabs = [ opened w "A"; opened w "B"; opened w "C" ]
    let before = mutations w.Driver |> List.length
    List.zip tabs ids |> List.iter (fun (t, id) -> observe w.Driver t id (sprintf "observed by %s" t.Name))
    // The same moment: every tab sends before any answer is processed.
    List.zip tabs ids |> List.iter (fun (t, id) -> send w.Driver t (Limen.Event("save", Some id, None)))
    settle w.Driver tabs
    List.zip tabs ids
    |> List.iter (fun (t, id) ->
        expectState t id "Saved"
        Expect.equal (count (sprintf "observed by %s" t.Name) (githubEvents w id)) 1 "confirmed exactly once"
        ids |> List.filter ((<>) id) |> List.iter (fun other -> expectState t other "Saved"))
    let inputs = mutationInputs w.Driver |> List.skip before
    for input in inputs do
        Expect.isTrue (match field "expectedHeadOid" input with Some(JString h) -> h.Length = 40 | _ -> false) "every mutation names an expected head"
        let paths =
            match field "fileChanges" input |> Option.bind (field "additions") with
            | Some(JArray adds) -> adds |> List.choose (field "path" >> function Some(JString p) -> Some p | _ -> None)
            | _ -> []
        Expect.isTrue (ids |> List.exists (fun id -> paths |> List.forall (fun p -> p.StartsWith(prefix w id)))) "paths stay inside one investigation"
    let ms = markers w.Driver
    Expect.equal (List.distinct ms).Length ms.Length "write markers are unique"
    for t in tabs do
        for KeyValue(_, s) in t.Model.Sessions do
            Expect.isTrue (w.Driver.Provider.Reachable s.BaseCommit) "every confirmed commit stays reachable"

let pat03 (w: World) =
    let id = seed w 1 |> List.head
    let a = opened w "A"
    let other = w.Driver.Provider.OtherWriter [ "notes/unrelated.md", "unrelated\n" ]
    let manifestBefore = w.Driver.Provider.FileAt other (prefix w id + "manifest.json")
    observe w.Driver a id "stale attempt"
    let replyCount = w.Driver.Net.Replies.Length
    save w.Driver a id
    let firstMutation = w.Driver.Net.Replies |> List.skip replyCount |> List.find (fun (r, _) -> r.Method = "POST")
    Expect.equal (classifyCommit (snd firstMutation)) StaleHead "GitHub's refusal is classified as a stale head"
    Expect.equal (w.Driver.Provider.FileAt other (prefix w id + "manifest.json")) manifestBefore "nothing was written by the refused mutation"
    expectState a id "Saved"

let pat03Faults (w: World) (fault: FakeGitHub.CommitFault) (want: string) (cause: FailureCause) =
    let id = seed w 1 |> List.head
    let a = opened w "A"
    w.Fake.Value(fun s -> { s with Behaviour = { s.Behaviour with NextCommitFault = Some fault } })
    observe w.Driver a id "refused"
    save w.Driver a id
    expectState a id want
    Expect.equal (session a id).Save (SaveFailed cause) "the cause is the one determined"
    Expect.isNonEmpty (session a id).Unsaved "the change is kept"

let pat03Network (w: World) =
    let id = seed w 1 |> List.head
    let a, b = opened w "A", opened w "B"
    observe w.Driver a id "network"
    w.Driver.Net.Mutations <- [ DropRequest ]
    w.Driver.AutoTimers <- false
    save w.Driver a id
    expectState a id "Unknown commit outcome"
    Expect.notEqual (saveState a id) "Conflict" "a network failure is not a conflict"
    w.Driver.Net.Online <- false
    send w.Driver a (Limen.Event("net", None, Some "offline"))
    observe w.Driver a id "offline edit"
    send w.Driver b (Limen.Event("net", None, Some "offline"))
    observe w.Driver b id "offline in B"
    let sentBefore = (mutations w.Driver).Length
    save w.Driver b id
    expectState b id "Offline"
    Expect.equal (mutations w.Driver).Length sentBefore "a save requested offline sends nothing"

let pat04 (w: World) =
    let ids = seed w 2
    let a = opened w "A"
    let i1, i2 = ids.[0], ids.[1]
    observe w.Driver a i2 "same tab, other investigation"
    save w.Driver a i2
    expectState a i2 "Saved"
    let sameTabCommit = (session a i2).BaseCommit
    let unrelated = w.Driver.Provider.OtherWriter [ "notes/outside.md", "outside the investigations\n" ]
    observe w.Driver a i1 "after unrelated commits"
    let before = (mutations w.Driver).Length
    save w.Driver a i1
    expectState a i1 "Saved"
    Expect.isLessThanOrEqual ((mutations w.Driver).Length - before) 4 "at most three automatic rebases"
    Expect.isTrue (w.Driver.Provider.Reachable sameTabCommit && w.Driver.Provider.Reachable unrelated) "unrelated commits stay on the branch"
    Expect.equal (w.Driver.Provider.FileAt (w.Driver.Provider.Head()) "notes/outside.md") (Some "outside the investigations\n") "unchanged"

let private hypothesis (w: World) (t: Tab) id =
    goTo w.Driver t id
    fill w.Driver t "hyp.statement" "The filament broke"
    send w.Driver t (Limen.Event("hyp.add", None, None))
    save w.Driver t id
    (session t id).Current.Value.Entities |> Map.toList |> List.find (fun (_, e) -> e.Kind = "hypothesis") |> fst

let private transition (w: World) (t: Tab) id hypothesisId target =
    goTo w.Driver t id
    send w.Driver t (Limen.Event("select", Some hypothesisId, None))
    fill w.Driver t "transition.reason" ("moved to " + target)
    send w.Driver t (Limen.Event("transition", Some("HypothesisTransitioned|" + target), None))

let pat05 (w: World) =
    let id = seed w 1 |> List.head
    let a, b = opened w "A", opened w "B"
    observe w.Driver a id "A's unsaved change"
    observe w.Driver b id "B's saved change"
    save w.Driver b id
    let bCommit = (session b id).BaseCommit
    let before = (mutations w.Driver).Length
    save w.Driver a id
    expectState a id "Conflict"
    Expect.equal (session a id).Unsaved.Length 1 "the unsaved change is kept"
    Expect.equal ((mutations w.Driver).Length - before) 1 "nothing is sent after the refusal"
    event w.Driver a "conflict.review" None None
    Expect.isTrue (session a id).Conflict.Value.Reviewing "review shows both versions"
    Expect.isSome (session a id).Conflict.Value.Theirs "the GitHub version is read for review"
    Expect.equal ((mutations w.Driver).Length - before) 1 "review sends nothing"
    event w.Driver a "conflict.reapply" None None
    expectState a id "Unsaved"
    Expect.isTrue (w.Driver.Provider.Reachable bCommit) "B's commit is still reachable"
    save w.Driver a id
    expectState a id "Saved"
    let events = githubEvents w id
    Expect.isTrue (events.Contains "A's unsaved change" && events.Contains "B's saved change") "both writers' changes are on GitHub"
    // A refused command: A supersedes the hypothesis while B falsifies it.
    let h = hypothesis w a id
    event w.Driver b "refresh" None None
    transition w a id h "superseded"
    transition w b id h "falsified"
    save w.Driver b id
    save w.Driver a id
    expectState a id "Conflict"
    event w.Driver a "conflict.reapply" None None
    Expect.isNonEmpty (session a id).Refused "refused commands are listed with the engine's reason"
    expectState a id "Saved"
    // Discard asks for confirmation first.
    observe w.Driver a id "A again"
    event w.Driver b "refresh" None None
    observe w.Driver b id "B again"
    save w.Driver b id
    save w.Driver a id
    expectState a id "Conflict"
    event w.Driver a "conflict.discard" None None
    expectState a id "Conflict"
    Expect.isTrue (session a id).Conflict.Value.ConfirmDiscard "discard asks for confirmation"
    event w.Driver a "conflict.discard.confirm" None None
    expectState a id "Saved"
    Expect.isTrue (w.Driver.Provider.Reachable (session b id).BaseCommit) "B's commit is still reachable"

let private unknownSetup (w: World) =
    let id = seed w 1 |> List.head
    let a = opened w "A"
    observe w.Driver a id "uncertain"
    id, a

let pat06Applied (w: World) =
    let id, a = unknownSetup w
    w.Driver.Net.Mutations <- [ DropAnswer ]
    w.Driver.AutoTimers <- false
    let before = (mutations w.Driver).Length
    save w.Driver a id
    expectState a id "Unknown commit outcome"
    Expect.equal ((mutations w.Driver).Length - before) 1 "no second mutation before reconciliation"
    w.Driver.AutoTimers <- true
    settle w.Driver [ a ]
    expectState a id "Saved"
    Expect.equal ((mutations w.Driver).Length - before) 1 "the applied mutation is confirmed, not resent"
    Expect.equal (count "uncertain" (githubEvents w id)) 1 "written exactly once"

/// GitHub applied the commit, its answer was lost, and the first reconciliation read does not see it yet.
let pat06Lagging (w: World) =
    let id, a = unknownSetup w
    w.Fake.Value(fun s -> { s with Behaviour = { s.Behaviour with LaggingCompares = 1 } })
    w.Driver.Net.Mutations <- [ DropAnswer ]
    save w.Driver a id
    expectState a id "Saved"
    Expect.equal (count "uncertain" (githubEvents w id)) 1 "written exactly once, never reported as a conflict"

let pat06NotApplied (w: World) =
    let id, a = unknownSetup w
    w.Driver.Net.Mutations <- [ DropRequest ]
    let before = (mutations w.Driver).Length
    save w.Driver a id
    expectState a id "Saved"
    Expect.equal ((mutations w.Driver).Length - before) 2 "sent again once reconciliation showed nothing was written"
    Expect.equal (count "uncertain" (githubEvents w id)) 1 "written exactly once"

let pat06Moved (w: World) =
    let id, a = unknownSetup w
    w.Driver.Net.Mutations <- [ DropRequest ]
    w.Driver.AutoTimers <- false
    save w.Driver a id
    expectState a id "Unknown commit outcome"
    w.Driver.Provider.OtherWriter [ "notes/moved.md", "moved\n" ] |> ignore
    w.Driver.AutoTimers <- true
    settle w.Driver [ a ]
    expectState a id "Saved"

let pat06Mismatch (w: World) =
    let id, a = unknownSetup w
    w.Fake.Value(fun s -> { s with Behaviour = { s.Behaviour with CorruptNextApply = true } })
    w.Driver.Net.Mutations <- [ DropAnswer ]
    save w.Driver a id
    expectState a id "Save failed"
    Expect.equal (session a id).Save (SaveFailed ReadBackMismatch) "a marker with different bytes is never reported as saved"

let pat06Rewritten (w: World) =
    let id, a = unknownSetup w
    w.Driver.Net.Mutations <- [ DropAnswer ]
    w.Driver.AutoTimers <- false
    save w.Driver a id
    w.Fake.Value FakeGitHub.rewriteHistory
    w.Driver.AutoTimers <- true
    settle w.Driver [ a ]
    Expect.equal (session a id).Save (SaveFailed HistoryRewritten) "rewritten history is a failure, not a save"

let pat06ReadFails (w: World) =
    let id, a = unknownSetup w
    w.Driver.Net.Mutations <- [ DropRequest ]
    w.Driver.Net.FailReads <- true
    let readsBefore = (reads "/compare/" w.Driver).Length
    save w.Driver a id
    expectState a id "Unknown commit outcome"
    Expect.equal ((reads "/compare/" w.Driver).Length - readsBefore) 3 "three automatic reconciliation reads"
    let indicator = View.indicator a.Model (session a id)
    Expect.isTrue indicator.CanCheckAgain "Check again is offered after the automatic reads"
    Expect.isFalse indicator.CanSave "Save is not offered"
    w.Driver.Net.FailReads <- false
    event w.Driver a "checkAgain" None None
    expectState a id "Saved"

let private sixtySeconds (w: World) (configure: Network -> unit) (allowed: string list) =
    let id, a = unknownSetup w
    configure w.Driver.Net
    let m0, r0, c0 = (mutations w.Driver).Length, (reads "/compare/" w.Driver).Length, w.Driver.Net.Clock
    save w.Driver a id
    Expect.isLessThanOrEqual (w.Driver.Net.Clock - c0) 60000 "counted within 60 seconds"
    Expect.isLessThanOrEqual ((mutations w.Driver).Length - m0) 4 "at most four commit mutations"
    Expect.isLessThanOrEqual ((reads "/compare/" w.Driver).Length - r0) 4 "at most four reconciliation reads"
    Expect.contains allowed (saveState a id) "ends Offline, Unknown commit outcome or Save failed, never Saving"
    w.Driver.Net.EveryMutation <- None
    w.Driver.Net.FailReads <- false
    w.Driver.Net.Online <- true

let pat08 (w: World) =
    sixtySeconds w (fun n -> n.Online <- false) [ "Offline" ]

let pat08Dropped (w: World) =
    sixtySeconds w (fun n -> n.EveryMutation <- Some DropRequest; n.FailReads <- true) [ "Unknown commit outcome" ]

let pat08Lost (w: World) =
    sixtySeconds w (fun n -> n.EveryMutation <- Some DropRequest) [ "Save failed"; "Unknown commit outcome" ]

let pat08Advancing (w: World) =
    sixtySeconds w (fun n -> n.EveryMutation <- Some AdvanceFirst) [ "Save failed" ]

let pat10 (w: World) =
    let ids = seed w 2
    let a, b, c = opened w "A", opened w "B", opened w "C"
    observe w.Driver a ids.[0] "confirmed from A"
    save w.Driver a ids.[0]
    observe w.Driver b ids.[1] "confirmed from B"
    save w.Driver b ids.[1]
    observe w.Driver c ids.[0] "never confirmed from C"
    let d = opened w "D"
    for id in ids do
        expectState d id "Saved"
        Expect.equal (session d id).BaseCommit (w.Driver.Provider.Head()) "loaded at the latest commit"
    let all = ids |> List.map (fun id -> (session d id).BaseFiles.Value.Events) |> String.concat ""
    Expect.isTrue (all.Contains "confirmed from A" && all.Contains "confirmed from B") "every confirmed change"
    Expect.isFalse (all.Contains "never confirmed from C") "no unconfirmed change"

let scenarios (mk: unit -> World) (liveCapable: bool) =
    let run name f = testCase name (fun () -> f (mk ()))
    let fakeOnly name f = if liveCapable then [] else [ run name f ]
    [ run "PAT-02 concurrent GitHub commits from three tabs" pat02
      run "PAT-03 stale head is classified and writes nothing" pat03
      yield! fakeOnly "PAT-03 a refused credential is Save failed" (fun w -> pat03Faults w FakeGitHub.Unauthorized "Save failed" CredentialRefused)
      yield! fakeOnly "PAT-03 a permission refusal is Save failed" (fun w -> pat03Faults w FakeGitHub.Forbidden "Save failed" PermissionDenied)
      yield! fakeOnly "PAT-03 a rate limit is Save failed" (fun w -> pat03Faults w FakeGitHub.RateLimited "Save failed" RateLimited)
      yield! fakeOnly "PAT-03 a validation refusal is Save failed" (fun w -> pat03Faults w FakeGitHub.Invalid "Save failed" (Rejected "A path is not valid"))
      run "PAT-03 a network failure is never a stale head or conflict" pat03Network
      run "PAT-04 automatic rebase over unrelated changes" pat04
      run "PAT-05 same-investigation conflict preservation" pat05
      run "PAT-06 lost answer after GitHub applied it" pat06Applied
      run "PAT-06 lost request, head unchanged" pat06NotApplied
      run "PAT-06 lost request, head moved" pat06Moved
      yield! fakeOnly "PAT-06 applied but not yet visible to reconciliation" pat06Lagging
      yield! fakeOnly "PAT-06 marker with different bytes" pat06Mismatch
      yield! fakeOnly "PAT-06 rewritten history" pat06Rewritten
      run "PAT-06 failing reconciliation reads" pat06ReadFails
      run "PAT-08 60 s with every request failing as if offline" pat08
      run "PAT-08 60 s with mutations and reads failing while online" pat08Dropped
      run "PAT-08 60 s with every mutation's answer lost" pat08Lost
      run "PAT-08 60 s with the branch advancing before every attempt" pat08Advancing
      run "PAT-10 reopening from GitHub" pat10 ]

let fakeWorld () =
    let p, _, update = fakeProvider "causa-test" "data" "test-token"
    { Driver = driver p; Workspace = workspaceName (); Fake = Some update; Mode = "fake" }

let tests = testList "persistence acceptance (fake)" (scenarios fakeWorld false)

let liveTests () =
    match Environment.GetEnvironmentVariable "CAUSA_WRITE_TOKEN" with
    | null | "" -> None
    | token ->
        let repo = Environment.GetEnvironmentVariable "CAUSA_WRITE_REPO" |> Option.ofObj |> Option.defaultValue "kemiller2002/causa-write-test"
        let owner, name = match repo.Split('/') with [| o; n |] -> o, n | _ -> failwith "CAUSA_WRITE_REPO must be owner/name"
        let branch = Environment.GetEnvironmentVariable "CAUSA_WRITE_BRANCH" |> Option.ofObj |> Option.defaultValue "main"
        let mk () = { Driver = driver (liveProvider owner name branch token); Workspace = workspaceName (); Fake = None; Mode = "live" }
        Some(testSequenced (testList "persistence acceptance (live)" (scenarios mk true)))
