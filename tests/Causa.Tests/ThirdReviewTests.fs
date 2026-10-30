/// Regression tests from the third independent review (KD-024): its reproductions E1-E3, kept as written
/// except for naming. E1 and E2 failed on main 1298489; E3 guards a deleted investigation from being re-created.
module Causa.Tests.ThirdReviewTests

open System
open Expecto
open Causa.Persistence
open Causa.App
open Causa.Tests.Harness

let private stepOne (d: Driver) =
    match d.Queue with
    | (tab, Limen.Http(id, r)) :: rest ->
        d.Queue <- rest
        let reply = execute d.Provider d.Net tab.Name r
        send d tab (Limen.HttpResult(id, reply))
        true
    | _ :: rest -> d.Queue <- rest; true
    | [] -> false

let private githubEvents (d: Driver) ws id =
    d.Provider.FileAt (d.Provider.Head()) (Causa.Domain.FileSet.investigationPrefix ws id + "events.ndjson") |> Option.defaultValue ""

let setupSeed () =
    let p, _, _ = fakeProvider "causa-test" "data" "test-token"
    let d = driver p
    let ws = workspaceName ()
    let s = newTab "S"
    connect d s ws
    let id = createInvestigation d s "Lamp"
    save d s id
    p, d, ws, id

/// E1: a conflict whose GitHub version is read again while the first read is still in flight.
let e1 () =
    let p, d, ws, id = setupSeed ()
    let a = newTab "A"
    connect d a ws
    let b = newTab "B"
    connect d b ws
    observe d a id "A change"
    observe d b id "B1"
    save d b id
    // A saves; stop as soon as A is in Conflict, with the read of the GitHub version (at h1) still queued.
    send d a (Limen.Event("save", Some id, None))
    while (session a id).Conflict.IsNone && stepOne d do ()
    Expect.equal (saveState a id) "Conflict" "A in conflict"
    let h1 = (session a id).Conflict.Value.Head
    // Another tab (separate driver, same GitHub) changes the investigation again: h2.
    let d2 = { driver p with Queue = [] }
    let c = newTab "C"
    connect d2 c ws
    observe d2 c id "C1 must survive"
    save d2 c id
    Expect.equal (saveState c id) "Saved" "C saved"
    // A clicks Read the GitHub version again while the first read is still in flight.
    send d a (Limen.Event("conflict.reread", None, None))
    let mutable reapplied = false
    while not reapplied && stepOne d do
        let ci = (session a id).Conflict
        match ci with
        | Some { Theirs = Some v; Head = h } when h <> h1 ->
            ignore <| sprintf "E1 window: Head=%s Theirs version=%d has C1=%b" (h.Substring(0, 7)) v.Investigation.Version (v.Files.Events.Contains "C1 must survive")
            send d a (Limen.Event("conflict.reapply", None, None))
            reapplied <- true
        | _ -> ()
    pump d
    Expect.isTrue reapplied "reached the window"
    ignore <| sprintf "E1 after reapply: state=%s base=%s" (saveState a id) ((session a id).BaseCommit.Substring(0, 7))
    save d a id
    ignore <| sprintf "E1 after save: state=%s" (saveState a id)
    let ev = githubEvents d ws id
    ignore <| sprintf "E1 GitHub head has 'A change'=%b 'B1'=%b 'C1 must survive'=%b" (ev.Contains "A change") (ev.Contains "B1") (ev.Contains "C1 must survive")
    Expect.isTrue (ev.Contains "C1 must survive") "the other writer's change C1 must remain on GitHub"

/// E2: a refresh that is in flight when the user edits and saves.
let e2 () =
    let p, d, ws, id = setupSeed ()
    let a = newTab "A"
    connect d a ws
    goTo d a id
    send d a (Limen.Event("refresh", None, None))
    stepOne d |> ignore // head read: h0; load(h0) file 1 now queued
    observe d a id "X confirmed"
    send d a (Limen.Event("save", Some id, None))
    // Let the save run to completion first; hold back the refresh's reads.
    let isLoadRead (r: Causa.Persistence.GitHub.Request) = r.Method = "GET" && r.Url.Contains "contents/"
    let held = ResizeArray()
    let rec runSave () =
        match d.Queue with
        | [] -> ()
        | (t, Limen.Http(cid, r)) as item :: rest when held.Count = 0 && isLoadRead r && (saveState a id = "Saving") && not (a.Model.Active.IsSome && false) ->
            // First queued item is the refresh's file read (queued before the save): hold it.
            d.Queue <- rest
            held.Add item
            runSave ()
        | _ -> if stepOne d then runSave ()
    runSave ()
    ignore <| sprintf "E2 after save: state=%s base=%s held=%d" (saveState a id) ((session a id).BaseCommit.Substring(0, 7)) held.Count
    let savedCommit = (session a id).BaseCommit
    d.Queue <- (List.ofSeq held) @ d.Queue
    pump d
    let s = session a id
    let hasX = s.Current.Value.Entities |> Map.exists (fun _ e -> e.Fields |> List.exists (fun (_, v) -> v = Causa.Domain.Json.JString "X confirmed"))
    ignore <| sprintf "E2 after refresh lands: state=%s base=%s (saved commit %s) view has X=%b" (saveState a id) (s.BaseCommit.Substring(0, 7)) (savedCommit.Substring(0, 7)) hasX
    observe d a id "Y"
    save d a id
    ignore <| sprintf "E2 next save: state=%s" (saveState a id)
    Expect.isTrue hasX "the view keeps the confirmed save"
    Expect.notEqual (saveState a id) "Conflict" "no false conflict with the tab's own save"


/// E3: another writer deletes the investigation while this tab has unsaved changes.
let e3 () =
    let p, _, update = fakeProvider "causa-test" "data" "test-token"
    let d = driver p
    let ws = workspaceName ()
    let s = newTab "S"
    connect d s ws
    let id = createInvestigation d s "Lamp"
    save d s id
    let a = newTab "A"
    connect d a ws
    observe d a id "A change"
    let pre = Causa.Domain.FileSet.investigationPrefix ws id
    update (fun st ->
        let st' = Causa.Fake.FakeGitHub.commitAs st "Delete investigation" []
        let c = st'.Commits.[st'.Head]
        let c' = { c with Tree = c.Tree |> Map.filter (fun k _ -> not (k.StartsWith pre)) }
        { st' with Commits = st'.Commits.Add(c.Sha, c') })
    let before = (mutations d).Length
    save d a id
    ignore <| sprintf "E3 after save: state=%s mutations=%d unsaved=%d theirsError=%A" (saveState a id) ((mutations d).Length - before) (session a id).Unsaved.Length ((session a id).Conflict |> Option.bind (fun c -> c.TheirsError))
    event d a "conflict.reapply" None None
    ignore <| sprintf "E3 after reapply: state=%s notice=%s" (saveState a id) a.Model.Notice
    Expect.equal (saveState a id) "Conflict" "deleted is a conflict"

let tests =
    testList "third review" [
        testCase "E1: a late read of an older GitHub version never resolves a conflict onto the wrong base" e1
        testCase "E2: a refresh that lands after a save never rolls the tab back" e2
        testCase "E3: a deleted investigation is a conflict, never silently re-created" e3
    ]
