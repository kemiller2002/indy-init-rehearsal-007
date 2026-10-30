/// Causa's application model and update. Pure: each message yields a new model and the effects to run.
module Causa.App.App

open System
open Causa.Domain
open Causa.Domain.Json
open Causa.Domain.Engine
open Causa.Domain.FileSet
open Causa.Persistence
open Causa.Persistence.GitHub
open Causa.App.Limen

type Route =
    | Home
    | Open of string
    | Help
    | Settings
    | Administration

type Connectivity =
    | Disconnected
    | Connecting
    | Connected
    | Denied of string
    | Incompatible of string
    | Unreachable of string

type Loading =
    | Pending
    | LoadFailed of string
    | Ready

type ConflictInfo =
    { Head: string
      Theirs: Verified option
      /// GitHub's confirmation of the version in Theirs, from the verified load that read it.
      TheirsConfirmation: Confirmation option
      TheirsError: string option
      Reviewing: bool
      ConfirmDiscard: bool }

type Session =
    { Id: string
      Load: Loading
      BaseCommit: string
      BaseFiles: Files option
      BaseState: Investigation option
      Current: Investigation option
      Unsaved: (Command * Envelope) list
      Sent: int
      Save: SaveState
      Conflict: ConflictInfo option
      Refused: (string * string) list
      Fault: string option }

type Request =
    | PRepository
    | PHead
    | PTree of head: string
    | PLoad of id: string * load: Load.Load
    /// A refresh, requested while the session was based on `fromBase`.
    | PRefreshHead of id: string * fromBase: string
    | PRefreshLoad of id: string * fromBase: string * load: Load.Load
    /// A conflict's GitHub version is read again, from the branch's current head (PERSIST-012).
    | PConflictHead of id: string
    | PTheirs of id: string * load: Load.Load
    | PSave of id: string

type Model =
    { Form: Map<string, string>
      Connection: Connection option
      Connectivity: Connectivity
      CanPush: bool
      Head: string option
      Online: bool
      Route: Route
      Sessions: Map<string, Session>
      Order: string list
      SessionId: string
      Sequence: int
      Active: (string * SaveProcess.Save) option
      Queue: string list
      Pending: Map<string, Request>
      NextId: int
      Selected: string option
      Polite: string
      Assertive: string
      Timer: (string * int) option
      TimerSeq: int
      ConfirmDisconnect: bool
      Notice: string }

let defaultApi = "https://api.github.com"

let init (sessionId: string) =
    { Form = Map.ofList [ "connect.branch", "main"; "ev.category", "directObservation"; "unk.materiality", "material"; "con.strength", "confirmedCause" ]
      Connection = None
      Connectivity = Disconnected
      CanPush = false
      Head = None
      Online = true
      Route = Home
      Sessions = Map.empty
      Order = []
      SessionId = sessionId
      Sequence = 0
      Active = None
      Queue = []
      Pending = Map.empty
      NextId = 1
      Selected = None
      Polite = ""
      Assertive = ""
      Timer = None
      TimerSeq = 0
      ConfirmDisconnect = false
      Notice = "" }

type Env = { Context: Context; Nonce: unit -> string }

let draft (m: Model) key = m.Form |> Map.tryFind key |> Option.defaultValue ""

let routeOf (l: Location) =
    let h = l.Hash.TrimStart('#')
    if h.StartsWith "/i/" then Open(h.Substring 3)
    elif h = "/help" then Help
    elif h = "/settings" then Settings
    elif h = "/admin" then Administration
    else Home

let private send (m: Model) (pending: Request) (r: GitHub.Request) : Model * Effect list =
    let id = sprintf "c%d" m.NextId
    { m with NextId = m.NextId + 1; Pending = m.Pending.Add(id, pending) }, [ Http(id, r) ]

let private updateSession (m: Model) (id: string) (f: Session -> Session) =
    match m.Sessions.TryFind id with
    | Some s -> { m with Sessions = m.Sessions.Add(id, f s) }
    | None -> m

let titleOf (s: Session) = s.Current |> Option.map (fun i -> i.Title) |> Option.defaultValue "Investigation"

let private announce (m: Model) (s: Session) =
    let text = sprintf "%s: %s" (titleOf s) (SaveState.name s.Save)
    match s.Save with
    | Conflict | UnknownOutcome | SaveFailed _ -> { m with Assertive = text }
    | _ -> { m with Polite = text }

/// Feeds the save-state machine. An illegal input leaves the state unchanged and records the typed fault.
let private applyInputs (m: Model) (id: string) (inputs: SaveInput list) =
    match m.Sessions.TryFind id with
    | None -> m
    | Some s0 ->
        let s =
            inputs
            |> List.fold
                (fun (s: Session) input ->
                    let confirmation = match input with Confirmed(c, _, _) | Discarded c | Reapplied(c, _, _) | Opened c -> Some c | _ -> None
                    match confirmation with
                    | Some c when Confirmation.investigationId c <> id ->
                        { s with Fault = Some(sprintf "%s in %s: the confirmation belongs to another investigation" (SaveState.inputName input) (SaveState.name s.Save)) }
                    | _ ->
                        match SaveState.step s.Save input with
                        | Ok next -> { s with Save = next }
                        | Error f -> { s with Fault = Some(sprintf "%s in %s: %s" f.Input f.From f.Reason) })
                s0
        let m' = { m with Sessions = m.Sessions.Add(id, s) }
        if SaveState.name s.Save <> SaveState.name s0.Save then announce m' s else m'

/// Starts a verified load of the investigation at `commit`; `pending` names what the load is for.
let private loadRequest (m: Model) (pending: string * Load.Load -> Request) (id: string) (commit: string) =
    let load, request = Load.start m.Connection.Value id commit
    send m (pending(id, load)) request

let private emptySession id commit =
    { Id = id; Load = Pending; BaseCommit = commit; BaseFiles = None; BaseState = None; Current = None; Unsaved = []; Sent = 0
      Save = Unsaved; Conflict = None; Refused = []; Fault = None }

// ---- Saving ----

let rec private handleNext (env: Env) (m: Model) (id: string) (next: SaveProcess.Next) : Model * Effect list =
    match next with
    | SaveProcess.Send(s, r, inputs) ->
        let m1 = applyInputs { m with Active = Some(id, s); Sequence = s.Sequence } id inputs
        send m1 (PSave id) r
    | SaveProcess.Wait(s, delay, inputs) ->
        let timerId = sprintf "t%d" (m.TimerSeq + 1)
        applyInputs { m with Active = Some(id, s); Sequence = s.Sequence; TimerSeq = m.TimerSeq + 1; Timer = Some(timerId, delay) } id inputs, []
    | SaveProcess.AwaitUser(s, inputs) -> applyInputs { m with Active = Some(id, s); Sequence = s.Sequence; Timer = None } id inputs, []
    | SaveProcess.Finish(outcome, inputs) ->
        let m1 = applyInputs { m with Active = None; Timer = None } id inputs
        let m2, effects =
            match outcome with
            | SaveProcess.Done c ->
                let m' =
                    updateSession m1 id (fun s ->
                        let remaining = s.Unsaved |> List.skip (min s.Sent s.Unsaved.Length)
                        let verified = FileSet.verify id (Confirmation.files c) |> Result.toOption
                        { s with
                            BaseCommit = Confirmation.commit c
                            BaseFiles = Some(Confirmation.files c)
                            BaseState = verified |> Option.map (fun v -> v.Investigation)
                            Unsaved = remaining
                            Sent = 0 })
                let remain = m'.Sessions.[id].Unsaved |> List.isEmpty |> not
                applyInputs m' id [ Confirmed(c, remain, m'.Online) ], []
            | SaveProcess.ConflictAt h ->
                let m' =
                    updateSession m1 id (fun s ->
                        { s with Sent = 0; Conflict = Some { Head = h; Theirs = None; TheirsConfirmation = None; TheirsError = None; Reviewing = false; ConfirmDiscard = false } })
                loadRequest m' PTheirs id h
            | SaveProcess.Stopped -> updateSession m1 id (fun s -> { s with Sent = 0 }), []
        let m3, more = startNext env m2
        m3, effects @ more

and startNext (env: Env) (m: Model) : Model * Effect list =
    match m.Active, m.Queue, m.Connection with
    | None, _ :: rest, Some _ when not m.Online ->
        // Known offline: no request is sent; a waiting save stays Offline until the user saves again (PERSIST-018).
        startNext env { m with Queue = rest }
    | None, id :: rest, Some conn ->
        let m1 = { m with Queue = rest }
        match m1.Sessions.TryFind id with
        | Some({ Save = (Unsaved | Offline | SaveFailed _) } as s) when not s.Unsaved.IsEmpty && s.Current.IsSome ->
            let m1 = applyInputs { m1 with Notice = "" } id [ SaveRequested true ]
            let inv = s.Current.Value
            let added = s.Unsaved |> List.map snd
            let baseEvents = s.BaseFiles |> Option.map (fun f -> f.Events) |> Option.defaultValue ""
            let files = FileSet.build baseEvents inv added
            let m2 = updateSession m1 id (fun s -> { s with Sent = added.Length })
            let headline = sprintf "Causa: %s (version %d)" inv.Title inv.Version
            let next =
                SaveProcess.start conn id (investigationPrefix conn.Workspace id) s.BaseCommit s.BaseFiles files added.Length m.SessionId m.Sequence headline
            handleNext env m2 id next
        | _ -> startNext env m1
    | _ -> m, []

let private requestSave (env: Env) (m: Model) (id: string) =
    match m.Sessions.TryFind id with
    | Some s when s.Load = Ready && not s.Unsaved.IsEmpty ->
        match s.Save with
        | Unsaved | Offline | SaveFailed _ when not m.CanPush ->
            { m with Notice = "This credential can read the repository but cannot write to it. Reconnect with a credential that can write." }, []
        | Unsaved | Offline | SaveFailed _ when not m.Online -> applyInputs { m with Notice = "" } id [ SaveRequested false ], []
        | Unsaved | Offline | SaveFailed _ when List.contains id m.Queue -> m, []
        | Unsaved | Offline | SaveFailed _ ->
            // Saves run one at a time; a waiting one stays as it is (nothing is being sent for it) until it starts.
            let waiting = m.Active.IsSome
            startNext env { m with Notice = (if waiting then "This save starts when the save in progress finishes." else ""); Queue = m.Queue @ [ id ] }
        | _ -> m, []
    | _ -> m, []

// ---- Editing ----

let private record (env: Env) (m: Model) (id: string) (cmd: Command) (clear: string list) =
    match m.Sessions.TryFind id with
    | Some s when s.Conflict.IsSome -> { m with Notice = "Resolve the conflict before making more changes." }
    | Some s when s.Load = Ready ->
        match decide env.Context id s.Current cmd with
        | Ok(envelope, inv) ->
            let m1 =
                updateSession m id (fun s -> { s with Current = Some inv; Unsaved = s.Unsaved @ [ cmd, envelope ]; Refused = [] })
            let m2 = applyInputs m1 id [ Edited m.Online ]
            { m2 with Form = clear |> List.fold (fun f k -> Map.remove k f) m2.Form; Notice = "" }
        | Error reason -> { m with Notice = "Not recorded: " + reason }
    | _ -> m

let private payload (fields: (string * Json) list) = JObject fields

let private optText (m: Model) key = let v = (draft m key).Trim() in if v = "" then JNull else JString v

let private required (m: Model) (keys: string list) = keys |> List.forall (fun k -> (draft m k).Trim() <> "")

let private current (m: Model) = match m.Route with Open id -> Some id | _ -> None

let private command (env: Env) (m: Model) (name: string) (key: string option) : Model =
    let newId = env.Context.NewId
    let t k = JString((draft m k).Trim())
    match current m with
    | None -> m
    | Some id ->
        let go eventType fields clear needed =
            if required m needed then record env m id { EventType = eventType; Payload = payload fields } clear
            else { m with Notice = "Fill in the required fields first." }
        match name, key with
        | "obs.add", _ -> go "ObservationRecorded" [ "observationId", JString(newId ()); "original", t "obs.text"; "observedAt", JNull ] [ "obs.text" ] [ "obs.text" ]
        | "hyp.add", _ ->
            go "HypothesisRecorded" [ "hypothesisId", JString(newId ()); "statement", t "hyp.statement"; "mechanism", optText m "hyp.mechanism" ] [ "hyp.statement"; "hyp.mechanism" ] [ "hyp.statement" ]
        | "ev.add", _ ->
            go
                "EvidenceAdmitted"
                [ "evidenceId", JString(newId ()); "contentKind", JString "text"; "content", t "ev.content"; "category", t "ev.category"
                  "source", t "ev.source"; "capturedAt", JNull ]
                [ "ev.content"; "ev.source" ]
                [ "ev.content"; "ev.source"; "ev.category" ]
        | "rel.add", _ ->
            go
                "EvidenceRelated"
                [ "relationshipId", JString(newId ()); "evidenceId", t "rel.evidence"; "hypothesisId", t "rel.hypothesis"; "kind", t "rel.kind"; "rationale", optText m "rel.rationale" ]
                [ "rel.rationale" ]
                [ "rel.evidence"; "rel.hypothesis"; "rel.kind" ]
        | "unk.add", _ -> go "UnknownIdentified" [ "unknownId", JString(newId ()); "question", t "unk.question"; "materiality", t "unk.materiality" ] [ "unk.question" ] [ "unk.question"; "unk.materiality" ]
        | "cause.add", _ ->
            go
                "CausalAssertionRecorded"
                [ "causeId", JString(newId ()); "outcome", t "cause.outcome"
                  "members", JArray [ JObject [ "hypothesisId", t "cause.hypothesis"; "role", JString "rootCause" ] ]; "mechanism", JNull ]
                [ "cause.outcome" ]
                [ "cause.outcome"; "cause.hypothesis" ]
        | "con.add", _ ->
            let causes =
                m.Sessions.[id].Current
                |> Option.map (fun i -> i.Entities |> Map.toList |> List.filter (fun (_, e) -> e.Kind = "cause" && e.State = Some "confirmed") |> List.map fst)
                |> Option.defaultValue []
            go
                "ConclusionRecorded"
                [ "conclusionId", JString(newId ()); "statement", t "con.statement"; "strength", t "con.strength"
                  "causeIds", jstrs (List.sort causes); "residualUncertainty", optText m "con.residual" ]
                [ "con.statement"; "con.residual" ]
                [ "con.statement"; "con.strength" ]
        | "transition", Some k when k.Contains "|" ->
            let parts = k.Split('|')
            let eventType, target = parts.[0], parts.[1]
            match m.Selected, eventType with
            | Some entityId, "HypothesisTransitioned" ->
                go eventType [ "hypothesisId", JString entityId; "to", JString target; "reason", t "transition.reason"; "evidenceIds", JArray [] ] [ "transition.reason" ] [ "transition.reason" ]
            | Some entityId, "CausalAssertionConfirmed" -> go eventType [ "causeId", JString entityId; "policy", JString "core-default/1" ] [] []
            | Some entityId, _ ->
                match stateChanges.TryFind eventType with
                | Some(kind, idField, copied) ->
                    let needs = if copied.IsEmpty then [] else [ "transition.reason" ]
                    go eventType ([ idField, JString entityId; stateKey kind, JString target ] @ (copied |> List.map (fun c -> c, t "transition.reason"))) [ "transition.reason" ] needs
                | None -> m
            | _ -> m
        | "life", Some target -> go "LifecycleChanged" [ "to", JString target; "reason", optText m "life.reason" ] [ "life.reason" ] []
        | _ -> m

// ---- Connecting and loading ----

let private safeName (s: string) = s <> "" && s |> Seq.forall (fun c -> Char.IsLetterOrDigit c || c = '-' || c = '_' || c = '.')

let private safeWorkspace (s: string) =
    s <> "" && not (s.Contains "..") && not (s.StartsWith "/") && s |> Seq.forall (fun c -> Char.IsLetterOrDigit c || c = '-' || c = '_' || c = '/' || c = '.')

/// The credential goes only to GitHub's API over HTTPS, or to this site's own origin (the test server's
/// same-origin GitHub stand-in); never to another host or over plaintext.
let sameOriginApi = "/github"

let private safeApi (s: string) = s = defaultApi || s = sameOriginApi

let private connect (env: Env) (m: Model) =
    let owner, repo, branch, workspace, token = draft m "connect.owner", draft m "connect.repo", draft m "connect.branch", draft m "connect.workspace", draft m "connect.token"
    let api = match (draft m "connect.api").Trim() with "" -> defaultApi | a -> a.TrimEnd('/')
    if not (safeName owner && safeName repo) then { m with Notice = "Enter the repository owner and name." }, []
    elif branch.Trim() = "" || branch.Contains ".." then { m with Notice = "Enter the branch to use." }, []
    elif not (safeWorkspace workspace) then { m with Notice = "Enter the workspace folder, for example workspaces/demo/my-run." }, []
    elif token.Trim() = "" then { m with Notice = "Enter a GitHub token. Causa keeps it only in this tab's memory." }, []
    elif not (safeApi api) then { m with Notice = "The GitHub API address must be https://api.github.com." }, []
    else
        let conn = { ApiBase = api; Owner = owner; Name = repo; Branch = branch.Trim(); Workspace = workspace.Trim().TrimEnd('/'); Token = token.Trim() }
        let m1 = { m with Connection = Some conn; Connectivity = Connecting; Form = m.Form.Remove "connect.token"; Notice = "" }
        send m1 PRepository (repository conn)

let private disconnect (m: Model) =
    // The write-marker sequence continues, so no marker is ever sent twice by this tab (PERSIST-015).
    { init m.SessionId with Form = m.Form |> Map.remove "connect.token"; Online = m.Online; Sequence = m.Sequence; Polite = "Disconnected" }

let private loadAll (m: Model) (commit: string) (ids: string list) =
    let m1 =
        { m with
            Connectivity = Connected
            Head = Some commit
            Sessions = ids |> List.fold (fun acc id -> Map.add id (emptySession id commit) acc) m.Sessions
            Order = m.Order @ (ids |> List.filter (fun id -> not (List.contains id m.Order))) }
    ids |> List.fold (fun (mm, effs) id -> let mm', e = loadRequest mm PLoad id commit in mm', effs @ e) (m1, [])

let private readError (answer: ReadAnswer<'a>) =
    match answer with
    | Missing -> "not found"
    | ReadDenied true -> "GitHub refused this credential"
    | ReadDenied false -> "this credential has no access"
    | ReadRateLimited -> "GitHub's rate limit was reached"
    | ReadOffline -> "unavailable offline"
    | ReadFailed e -> e
    | Read _ -> "unexpected"

/// Whether replacing the session with a newly loaded version would lose work: edits not yet confirmed,
/// a save in flight, or a conflict awaiting the user (PERSIST-019).
let private holdsWork (m: Model) (s: Session) =
    s.Current.IsSome && (not s.Unsaved.IsEmpty || (match s.Save with Saved _ -> false | _ -> true) || m.Active |> Option.exists (fst >> (=) s.Id))

let private onLoaded (m: Model) id (v: Verified, c: Confirmation) =
    match m.Sessions.TryFind id with
    | Some s when holdsWork m s ->
        { (updateSession m id (fun s -> { s with Load = Ready })) with
            Notice = "Your unsaved changes were kept; the GitHub version was not loaded over them." }, []
    | _ ->
        let m1 =
            updateSession m id (fun s ->
                { s with Load = Ready; BaseCommit = Confirmation.commit c; BaseFiles = Some v.Files; BaseState = Some v.Investigation; Current = Some v.Investigation
                         Unsaved = []; Sent = 0; Conflict = None; Refused = [] })
        applyInputs m1 id [ Opened c ], []

let private withTheirs (m: Model) id (f: ConflictInfo -> ConflictInfo) =
    updateSession m id (fun s -> { s with Conflict = s.Conflict |> Option.map f })

let private onReply (env: Env) (m: Model) (pending: Request) (reply: Reply) : Model * Effect list =
    match pending with
    | PRepository ->
        match classifyRead decodeRepository reply with
        | Read info -> send { m with CanPush = info.CanPush } PHead (head m.Connection.Value (env.Nonce()))
        | Missing -> { m with Connectivity = Denied "The repository was not found, or this credential cannot see it." }, []
        | ReadOffline -> { m with Connectivity = Unreachable "You are offline. Connect again when the connection returns." }, []
        | other -> { m with Connectivity = Denied("GitHub refused access: " + readError other + ".") }, []
    | PHead ->
        match classifyRead decodeHead reply with
        | Read h -> send m (PTree h) (tree m.Connection.Value h)
        | Missing -> { m with Connectivity = Denied "The branch was not found." }, []
        | ReadOffline -> { m with Connectivity = Unreachable "You are offline. Connect again when the connection returns." }, []
        | other -> { m with Connectivity = Denied("GitHub refused access: " + readError other + ".") }, []
    | PTree h ->
        match classifyRead (decodeTree m.Connection.Value.Workspace) reply with
        | Read ids -> loadAll m h ids
        | ReadFailed e -> { m with Connectivity = Incompatible e }, []
        | other -> { m with Connectivity = Unreachable(readError other) }, []
    | PLoad(id, load) ->
        match Load.onReply load reply (env.Context.Now()) with
        | Load.Next(load', request) -> send m (PLoad(id, load')) request
        | Load.Loaded(v, c) -> onLoaded m id (v, c)
        | Load.Refused e ->
            // A refused refresh keeps the loaded version; only a first load fails.
            updateSession m id (fun s ->
                { s with
                    Load = (if s.Current.IsSome then Ready else LoadFailed("Refused, because the data failed verification: " + e))
                    Fault = Some("The GitHub version failed verification: " + e) }), []
        | Load.NotRead other -> updateSession m id (fun s -> { s with Load = (if s.Current.IsSome then Ready else LoadFailed(readError other)); Fault = Some(readError other) }), []
    | PRefreshLoad(id, fromBase, load) ->
        match Load.onReply load reply (env.Context.Now()), m.Sessions.TryFind id with
        | Load.Next(load', request), _ -> send m (PRefreshLoad(id, fromBase, load')) request
        // The session moved on (a save confirmed) while this refresh read an older version: never roll it back.
        | Load.Loaded _, Some s when s.BaseCommit <> fromBase -> m, []
        | Load.Loaded(v, c), _ -> onLoaded m id (v, c)
        | Load.Refused e, _ -> updateSession m id (fun s -> { s with Load = Ready; Fault = Some("The GitHub version failed verification: " + e) }), []
        | Load.NotRead other, _ -> updateSession m id (fun s -> { s with Load = Ready; Fault = Some(readError other) }), []
    | PRefreshHead(id, fromBase) ->
        match classifyRead decodeHead reply with
        | Read h -> loadRequest m (fun (id, load) -> PRefreshLoad(id, fromBase, load)) id h
        | other -> { m with Notice = "Could not read GitHub: " + readError other }, []
    | PConflictHead id ->
        match classifyRead decodeHead reply with
        | Read h -> loadRequest (withTheirs m id (fun ci -> { ci with Head = h; Theirs = None; TheirsConfirmation = None; TheirsError = None })) PTheirs id h
        | other -> withTheirs m id (fun ci -> { ci with TheirsError = Some(readError other) }), []
    | PTheirs(id, load) ->
        // Only the read of the conflict's current head may fill it; an older read that finishes late is dropped.
        let forCurrentHead (f: ConflictInfo -> ConflictInfo) (ci: ConflictInfo) = if ci.Head = Load.commit load then f ci else ci
        match Load.onReply load reply (env.Context.Now()) with
        | Load.Next(load', request) -> send m (PTheirs(id, load')) request
        | Load.Loaded(v, c) -> withTheirs m id (forCurrentHead (fun ci -> { ci with Theirs = Some v; TheirsConfirmation = Some c; TheirsError = None })), []
        | Load.Refused e -> withTheirs m id (forCurrentHead (fun ci -> { ci with TheirsError = Some e })), []
        | Load.NotRead other -> withTheirs m id (forCurrentHead (fun ci -> { ci with TheirsError = Some(readError other) })), []
    | PSave id ->
        match m.Active with
        | Some(activeId, save) when activeId = id -> handleNext env m id (SaveProcess.onReply save reply (env.Context.Now()) (env.Nonce()))
        | _ -> m, []

// ---- Conflict resolution (PERSIST-012) ----

let private reapply (env: Env) (m: Model) (id: string) =
    match m.Sessions.TryFind id with
    | Some({ Conflict = Some { Theirs = Some theirs; TheirsConfirmation = Some c; Head = h } } as s) ->
        let folder (state, accepted, refused) (cmd: Command, old: Envelope) =
            match decide env.Context id (Some state) cmd with
            | Ok(envelope, next) -> next, accepted @ [ cmd, envelope ], refused
            | Error reason -> state, accepted, refused @ [ old.EventType, reason ]
        let final, accepted, refused = s.Unsaved |> List.fold folder (theirs.Investigation, [], [])
        let m1 =
            updateSession m id (fun s ->
                { s with BaseCommit = Confirmation.commit c; BaseFiles = Some theirs.Files; BaseState = Some theirs.Investigation; Current = Some final
                         Unsaved = accepted; Refused = refused; Conflict = None })
        applyInputs m1 id [ Reapplied(c, not accepted.IsEmpty, m.Online) ]
    | _ -> { m with Notice = "The GitHub version is still being read. Try again in a moment." }

let private discard (env: Env) (m: Model) (id: string) =
    match m.Sessions.TryFind id with
    | Some({ Conflict = Some { Theirs = Some theirs; TheirsConfirmation = Some c; Head = h; ConfirmDiscard = true } }) ->
        let m1 =
            updateSession m id (fun s ->
                { s with BaseCommit = Confirmation.commit c; BaseFiles = Some theirs.Files; BaseState = Some theirs.Investigation; Current = Some theirs.Investigation
                         Unsaved = []; Refused = []; Conflict = None })
        applyInputs m1 id [ Discarded c ]
    | _ -> m

let private withConflict (m: Model) (f: ConflictInfo -> ConflictInfo) =
    match current m with
    | Some id -> updateSession m id (fun s -> { s with Conflict = s.Conflict |> Option.map f })
    | None -> m

/// Asks GitHub again for the current version of `id`'s conflict.
let private rereadConflict (env: Env) (m: Model) (id: string) =
    match m.Connection, m.Sessions.TryFind id with
    | Some conn, Some { Conflict = Some _ } ->
        send (withTheirs m id (fun ci -> { ci with TheirsError = None })) (PConflictHead id) (head conn (env.Nonce()))
    | _ -> m, []

/// Conflicts whose GitHub version could not be read are read again when the connection returns.
let private rereadConflicts (env: Env) (m: Model) =
    m.Sessions
    |> Map.toList
    |> List.filter (fun (_, s) -> s.Conflict |> Option.exists (fun c -> c.TheirsError.IsSome))
    |> List.fold (fun (mm, effs) (id, _) -> let mm', e = rereadConflict env mm id in mm', effs @ e) (m, [])

let private connectivity (env: Env) (m: Model) (online: bool) =
    let input = if online then ConnectivityRestored else ConnectivityLost
    let m1 = m.Order |> List.fold (fun acc id -> applyInputs acc id [ input ]) { m with Online = online }
    let m2, rereads = if online then rereadConflicts env m1 else m1, []
    match online, m2.Active with
    | true, Some(id, save) ->
        match SaveProcess.reconcileNow save true (env.Nonce()) with
        | Some next -> let m3, e = handleNext env { m2 with Timer = None } id next in m3, rereads @ e
        | None -> m2, rereads
    | _ -> m2, rereads

let private createInvestigation (env: Env) (m: Model) =
    match m.Head with
    | Some h when required m [ "new.title"; "new.problem" ] ->
        let id = env.Context.NewId()
        let cmd = { EventType = "InvestigationStarted"; Payload = JObject [ "title", JString((draft m "new.title").Trim()); "observationId", JString(env.Context.NewId()); "original", JString((draft m "new.problem").Trim()) ] }
        match decide env.Context id None cmd with
        | Ok(envelope, inv) ->
            let s = { emptySession id h with Load = Ready; Current = Some inv; Unsaved = [ cmd, envelope ]; Save = (if m.Online then Unsaved else Offline) }
            { m with
                Sessions = m.Sessions.Add(id, s)
                Order = m.Order @ [ id ]
                Form = m.Form.Remove("new.title").Remove("new.problem")
                Notice = ""
                Polite = sprintf "%s: %s" inv.Title (SaveState.name s.Save) }, id
        | Error e -> { m with Notice = "Not recorded: " + e }, ""
    | Some _ -> { m with Notice = "Give the investigation a title and describe the problem." }, ""
    | None -> { m with Notice = "Connect to a repository first." }, ""

/// Only a loaded investigation has a save state; one still loading, or refused, holds nothing to lose.
let isLoaded (s: Session) = s.Load = Ready

let anyUnsaved (m: Model) = m.Sessions |> Map.exists (fun _ s -> isLoaded s && SaveState.needsLeaveWarning s.Save)

let update (env: Env) (m: Model) (msg: Inbound) : Model * Effect list =
    match msg with
    | Initialize l -> { m with Route = routeOf l }, []
    | LocationChanged l -> { m with Route = routeOf l; Selected = None; Notice = "" }, []
    | HttpResult(cid, reply) ->
        match m.Pending.TryFind cid with
        | Some p -> onReply env { m with Pending = m.Pending.Remove cid } p reply
        | None -> m, []
    | Ignored -> m, []
    | Event(name, key, value) ->
        match name, key, value with
        | n, _, Some v when n.StartsWith "draft:" -> { m with Form = m.Form.Add(n.Substring 6, v) }, []
        | "connect", _, _ -> connect env m
        | "disconnect", _, _ -> (if anyUnsaved m then { m with ConfirmDisconnect = true }, [] else disconnect m, [])
        | "disconnect.confirm", _, _ -> disconnect m, []
        | "disconnect.cancel", _, _ -> { m with ConfirmDisconnect = false }, []
        | "net", _, Some v -> connectivity env m (v = "online")
        | "timer", _, Some t when m.Timer |> Option.exists (fst >> (=) t) ->
            match m.Active with
            | Some(id, save) ->
                match SaveProcess.reconcileNow save true (env.Nonce()) with
                | Some next -> handleNext env { m with Timer = None } id next
                | None -> { m with Timer = None }, []
            | None -> { m with Timer = None }, []
        | "save", Some id, _ -> requestSave env m id
        | "save", None, _ -> (match current m with Some id -> requestSave env m id | None -> m, [])
        | "checkAgain", _, _ ->
            match m.Active with
            | Some(id, save) ->
                match SaveProcess.reconcileNow save false (env.Nonce()) with
                | Some next -> handleNext env m id next
                | None -> m, []
            | None -> m, []
        | "refresh", _, _ ->
            match current m, m.Connection with
            | Some id, Some conn when (match m.Sessions.TryFind id with Some s -> not (holdsWork m s) | None -> false) ->
                send m (PRefreshHead(id, m.Sessions.[id].BaseCommit)) (head conn (env.Nonce()))
            | _ -> { m with Notice = "Save or resolve your changes before loading the GitHub version." }, []
        | "select", Some id, _ -> { m with Selected = (if m.Selected = Some id then None else Some id) }, []
        | "conflict.reread", _, _ -> (match current m with Some id -> rereadConflict env m id | None -> m, [])
        | "conflict.review", _, _ -> withConflict m (fun c -> { c with Reviewing = not c.Reviewing }), []
        | "conflict.reapply", _, _ -> (match current m with Some id -> reapply env m id, [] | None -> m, [])
        | "conflict.discard", _, _ -> withConflict m (fun c -> { c with ConfirmDiscard = true }), []
        | "conflict.discard.cancel", _, _ -> withConflict m (fun c -> { c with ConfirmDiscard = false }), []
        | "conflict.discard.confirm", _, _ -> (match current m with Some id -> discard env m id, [] | None -> m, [])
        | "new.create", _, _ ->
            match createInvestigation env m with
            | m', "" -> m', []
            | m', id -> { m' with Route = Open id }, [ Navigate("nav-" + id, "#/i/" + id) ]
        | "notice.dismiss", _, _ -> { m with Notice = "" }, []
        | _ -> command env m name key, []
