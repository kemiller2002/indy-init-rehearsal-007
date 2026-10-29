/// Projects the model into the flat view the page binds. The page renders only this projection.
module Causa.App.View

open System
open Causa.Domain.Json
open Causa.Domain.Engine
open Causa.Persistence
open Causa.App.Limen
open Causa.App.App

let humanize (s: string) =
    if String.IsNullOrEmpty s then s
    else
        let spaced = s |> Seq.mapi (fun i c -> if i > 0 && Char.IsUpper c then " " + string (Char.ToLowerInvariant c) else string c) |> String.concat ""
        string (Char.ToUpperInvariant spaced.[0]) + spaced.Substring 1

let causeText =
    function
    | RepositoryKeptChanging -> "GitHub refused the change because the repository kept changing during four attempts."
    | CredentialRefused -> "GitHub refused the credential."
    | PermissionDenied -> "This credential is not allowed to write to the repository."
    | RateLimited -> "GitHub's rate limit was reached. Wait a minute, then retry."
    | Rejected m -> "GitHub refused the change: " + m
    | ReadBackMismatch -> "GitHub holds different content than this tab sent."
    | HistoryRewritten -> "The branch history was rewritten on GitHub, so this change cannot be placed on it."

type Indicator =
    { Name: string
      Lozenge: string
      Glyph: string
      Text: string
      CanSave: bool
      CanRetry: bool
      CanReconnect: bool
      CanCheckAgain: bool
      InConflict: bool }

/// The words, glyph and actions of each save state (PERSIST-031, PERSIST-032).
let indicator (m: Model) (s: Session) : Indicator =
    let waitingForUser =
        match m.Active with
        | Some(id, save) when id = s.Id -> save.Phase = SaveProcess.WaitingToReconcile false
        | _ -> false
    let basic = { Name = SaveState.name s.Save; Lozenge = "unknown"; Glyph = "?"; Text = ""; CanSave = false; CanRetry = false; CanReconnect = false; CanCheckAgain = false; InConflict = false }
    match s.Save with
    | Saved c ->
        { basic with Lozenge = "ok"; Glyph = "✓"; Text = sprintf "Saved on GitHub at commit %s." ((Confirmation.commit c).Substring(0, 7)) }
    | Unsaved -> { basic with Lozenge = "attention"; Glyph = "●"; Text = "Not yet on GitHub. These changes exist only in this tab and are lost if it closes."; CanSave = m.Online }
    | Saving -> { basic with Lozenge = "unknown"; Glyph = "↻"; Text = "Sending this change to GitHub. It is not confirmed yet." }
    | Offline -> { basic with Lozenge = "attention"; Glyph = "⦸"; Text = "You are offline. These changes exist only in this tab and cannot be sent until the connection returns."; CanSave = m.Online }
    | Conflict ->
        { basic with
            Lozenge = "blocked"; Glyph = "⚠"; InConflict = true
            Text = "Another writer changed this investigation on GitHub. Nothing from this tab was written, and your changes are kept in this tab." }
    | UnknownOutcome ->
        { basic with
            Lozenge = "unknown"; Glyph = "?"; CanCheckAgain = waitingForUser
            Text =
                if waitingForUser then "The last attempt may or may not have reached GitHub. The automatic checks are finished; choose Check again."
                else "The last attempt may or may not have reached GitHub. Causa is checking GitHub before anything is sent again." }
    | SaveFailed cause ->
        let reconnect = match cause with CredentialRefused | PermissionDenied | HistoryRewritten -> true | _ -> false
        { basic with
            Lozenge = "blocked"; Glyph = "✕"; CanReconnect = reconnect; CanRetry = not reconnect && m.Online
            Text = causeText cause + " Your changes are kept in this tab." }

let private connectionLabel (m: Model) =
    let anyConflict = m.Sessions |> Map.exists (fun _ s -> s.Save = Conflict)
    match m.Connectivity with
    | _ when not m.Online -> "Offline"
    | Disconnected -> "Disconnected"
    | Connecting -> "Connecting"
    | Denied _ -> "Access denied"
    | Incompatible _ -> "Incompatible data"
    | Unreachable _ -> "Offline"
    | Connected when anyConflict -> "Connected · conflict"
    | Connected when m.Sessions |> Map.exists (fun _ s -> s.Load = Pending) -> "Connected · loading investigations"
    | Connected when m.Sessions.IsEmpty -> "Connected · no investigations yet"
    | Connected -> "Connected"

let private connectionDetail (m: Model) =
    match m.Connectivity, m.Connection with
    | Denied d, _ | Incompatible d, _ | Unreachable d, _ -> d
    | Connected, Some c -> sprintf "%s/%s · %s · %s" c.Owner c.Name c.Branch c.Workspace
    | _ -> ""

let private text name payload = match field name payload with Some(JString s) -> s | _ -> ""

let private eventSummary (e: Envelope) =
    let main =
        [ "title"; "statement"; "original"; "content"; "question"; "proposition"; "outcome"; "description"; "expectedOutcome"; "purpose"; "originalResult"; "reason"; "basis" ]
        |> List.map (fun k -> text k e.Payload)
        |> List.tryFind (fun v -> v <> "")
        |> Option.defaultValue ""
    match text "from" e.Payload, text "to" e.Payload with
    | "", _ -> main
    | f, t -> sprintf "%s → %s: %s" (humanize f) (humanize t) main

let private entityLabel (e: Entity) =
    let pick ks = ks |> List.map (fun k -> e.Fields |> List.tryFind (fst >> (=) k) |> Option.map snd) |> List.tryPick (function Some(JString s) -> Some s | _ -> None)
    pick [ "statement"; "original"; "content"; "question"; "proposition"; "outcome"; "description"; "expectedOutcome"; "purpose"; "originalResult" ] |> Option.defaultValue e.Id

let private fieldText (e: Entity) k = e.Fields |> List.tryFind (fst >> (=) k) |> Option.map snd |> function Some(JString s) -> s | _ -> ""

let private entities (inv: Investigation option) kind (selected: string option) =
    inv
    |> Option.map (fun i -> i.Entities |> Map.toList |> List.map snd |> List.filter (fun e -> e.Kind = kind) |> List.sortBy (fun e -> e.RecordedVersion))
    |> Option.defaultValue []
    |> List.map (fun e ->
        [ "id", VText e.Id
          "label", VText(entityLabel e)
          "state", VText(e.State |> Option.map humanize |> Option.defaultValue "")
          "hasState", VBool e.State.IsSome
          "detail",
          VText(
              match kind with
              | "evidence" -> sprintf "%s · %s" (humanize (fieldText e "category")) (fieldText e "source")
              | "unknown" | "assumption" | "contradiction" -> humanize (fieldText e "materiality")
              | "relationship" -> humanize (fieldText e "kind")
              | "conclusion" -> humanize (fieldText e "strength")
              | _ -> "")
          "selected", VBool(selected = Some e.Id)
          "pressed", VText(if selected = Some e.Id then "true" else "false") ])

let private events (s: Session) =
    let baseEvents =
        s.BaseFiles |> Option.map (fun f -> Causa.Domain.FileSet.parseEvents f.Events |> Result.defaultValue []) |> Option.defaultValue []
    let unsaved = s.Unsaved |> List.map snd
    (baseEvents |> List.map (fun e -> e, false)) @ (unsaved |> List.map (fun e -> e, true))
    |> List.rev
    |> List.map (fun (e, pending) ->
        [ "version", VText(string e.AggregateVersion)
          "type", VText(humanize e.EventType)
          "summary", VText(eventSummary e)
          "at", VText(e.RecordedAt.Replace("T", " ").Substring(0, 16) + " UTC")
          "pending", VBool pending ])

let private options (inv: Investigation option) kind =
    entities inv kind None |> List.map (fun fields -> fields |> List.filter (fun (k, _) -> k = "id" || k = "label"))

let draftKeys =
    [ "obs.text"; "hyp.statement"; "hyp.mechanism"; "ev.content"; "ev.source"; "ev.category"; "rel.evidence"; "rel.hypothesis"; "rel.kind"; "rel.rationale"
      "unk.question"; "unk.materiality"; "cause.outcome"; "cause.hypothesis"; "con.statement"; "con.strength"; "con.residual"; "transition.reason"; "life.reason"
      "rename.title" ]

let render (env: Env) (m: Model) : (string * ViewValue) list =
    let connected = m.Connectivity = Connected
    let session = match m.Route with Open id -> m.Sessions.TryFind id | _ -> None
    let inv = session |> Option.bind (fun s -> s.Current)
    let ind = session |> Option.map (indicator m)
    let notSaved = m.Order |> List.choose m.Sessions.TryFind |> List.filter (fun s -> SaveState.needsLeaveWarning s.Save)
    let selectedEntity = match inv, m.Selected with Some i, Some id -> i.Entities.TryFind id | _ -> None
    let transitions =
        match inv, selectedEntity, session with
        | Some i, Some e, Some s when s.Conflict.IsNone ->
            legalTargets env.Context i e |> List.map (fun (eventType, target) -> [ "key", VText(eventType + "|" + target); "label", VText("Mark " + (humanize target).ToLowerInvariant()) ])
        | _ -> []
    let lifecycle =
        match inv, session with
        | Some i, Some s when s.Conflict.IsNone ->
            legalLifecycle env.Context i
            |> List.map (fun t -> [ "key", VText t; "label", VText((humanize (t.Replace("Investigation", ""))).Trim()) ])
        | _ -> []
    let conflict = session |> Option.bind (fun s -> s.Conflict)
    let theirs = conflict |> Option.bind (fun c -> c.Theirs)
    let summary (i: Investigation option) =
        match i with
        | Some i -> sprintf "%s · version %d · %s · %d records" i.Title i.Version (humanize i.Lifecycle) i.Entities.Count
        | None -> "Not available"
    let mode =
        match m.Connection with
        | Some c when c.ApiBase <> defaultApi -> "Test provider at " + c.ApiBase + ". This is not GitHub."
        | _ -> ""
    let b name v = name, VBool v
    let t name v = name, VText v
    [
        t "connection" (connectionLabel m)
        t "connectionDetail" (connectionDetail m)
        b "connected" connected
        b "disconnected" (not connected)
        b "connecting" (m.Connectivity = Connecting)
        b "online" m.Online
        b "offline" (not m.Online)
        t "mode" mode
        b "hasMode" (mode <> "")
        t "notice" m.Notice
        b "hasNotice" (m.Notice <> "")
        t "polite" m.Polite
        t "assertive" m.Assertive
        b "confirmDisconnect" m.ConfirmDisconnect
        t "leaveGuard" (if anyUnsaved m then "on" else "off")
        t "timer" (match m.Timer with Some(id, ms) -> sprintf "%s:%d" id ms | None -> "")
        t "unsavedCount" (match notSaved.Length with 0 -> "" | 1 -> "1 investigation is not saved" | n -> sprintf "%d investigations are not saved" n)
        b "hasUnsavedSummary" (not notSaved.IsEmpty)
        "unsavedList", VList(notSaved |> List.map (fun s -> [ "id", VText s.Id; "title", VText(titleOf s); "state", VText(SaveState.name s.Save); "href", VText("#/i/" + s.Id) ]))
        // Routes
        b "routeHome" (m.Route = Home)
        b "routeHelp" (m.Route = Help)
        b "routeSettings" (m.Route = Settings)
        b "routeInvestigation" (session.IsSome)
        b "routeMissing" (match m.Route with Open id -> not (m.Sessions.ContainsKey id) | _ -> false)
        // Connect form
        t "f.owner" (draft m "connect.owner")
        t "f.repo" (draft m "connect.repo")
        t "f.branch" (draft m "connect.branch")
        t "f.workspace" (draft m "connect.workspace")
        t "f.api" (draft m "connect.api")
        t "f.token" (draft m "connect.token")
        // Dashboard
        b "noInvestigations" (connected && m.Sessions.IsEmpty)
        "investigations",
        VList(
            m.Order
            |> List.choose m.Sessions.TryFind
            |> List.map (fun s ->
                let i = indicator m s
                [ "id", VText s.Id
                  "title", VText(match s.Load, s.Current with
                                 | _, Some c -> c.Title
                                 | LoadFailed _, None -> "Investigation " + s.Id.Substring(0, 8)
                                 | _ -> "Loading investigation")
                  "problem", VText(s.Current |> Option.map (fun c -> c.OriginalProblem) |> Option.defaultValue "")
                  "lifecycle", VText(s.Current |> Option.map (fun c -> humanize c.Lifecycle) |> Option.defaultValue "")
                  "status", VText(match s.Load with
                                  | Pending -> "Loading from GitHub"
                                  | LoadFailed e when not m.Online -> "Unavailable offline (" + e + ")"
                                  | LoadFailed e -> e
                                  | Ready -> sprintf "%d hypotheses · %d evidence" (s.Current |> Option.map (fun c -> c.Entities |> Map.filter (fun _ e -> e.Kind = "hypothesis") |> Map.count) |> Option.defaultValue 0) (s.Current |> Option.map (fun c -> c.Entities |> Map.filter (fun _ e -> e.Kind = "evidence") |> Map.count) |> Option.defaultValue 0))
                  "ready", VBool(s.Load = Ready)
                  "saveState", VText(if s.Load = Ready then i.Name else "")
                  "lozenge", VText i.Lozenge
                  "glyph", VText i.Glyph
                  "href", VText("#/i/" + s.Id) ]))
        t "new.title" (draft m "new.title")
        t "new.problem" (draft m "new.problem")
        // Current investigation
        t "inv.id" (session |> Option.map (fun s -> s.Id) |> Option.defaultValue "")
        t "inv.title" (inv |> Option.map (fun i -> i.Title) |> Option.defaultValue "")
        t "inv.problem" (inv |> Option.map (fun i -> i.OriginalProblem) |> Option.defaultValue "")
        t "inv.lifecycle" (inv |> Option.map (fun i -> humanize i.Lifecycle) |> Option.defaultValue "")
        t "inv.version" (inv |> Option.map (fun i -> string i.Version) |> Option.defaultValue "")
        b "inv.ready" (session |> Option.exists (fun s -> s.Load = Ready))
        b "inv.loading" (session |> Option.exists (fun s -> s.Load = Pending))
        t "inv.loadError" (match session with Some { Load = LoadFailed e } -> (if m.Online then e else "Unavailable offline. This investigation was not loaded before the connection was lost.") | _ -> "")
        b "inv.failed" (match session with Some { Load = LoadFailed _ } -> true | _ -> false)
        b "inv.editable" (session |> Option.exists (fun s -> s.Load = Ready && s.Conflict.IsNone))
        t "save.state" (ind |> Option.map (fun i -> i.Name) |> Option.defaultValue "")
        t "save.lozenge" (ind |> Option.map (fun i -> i.Lozenge) |> Option.defaultValue "unknown")
        t "save.glyph" (ind |> Option.map (fun i -> i.Glyph) |> Option.defaultValue "")
        t "save.text" (ind |> Option.map (fun i -> i.Text) |> Option.defaultValue "")
        t "save.fresh" (match session with
                        | Some { Save = Saved c } -> "Last saved " + (Confirmation.confirmedAt c).Replace("T", " ").Substring(0, 19) + " UTC"
                        | Some s when s.BaseFiles.IsSome -> "Last confirmed on GitHub at commit " + s.BaseCommit.Substring(0, 7)
                        | _ -> "Never confirmed on GitHub")
        t "save.freshTime" (match session with Some { Save = Saved c } -> Confirmation.confirmedAt c | _ -> "")
        b "save.canSave" (ind |> Option.exists (fun i -> i.CanSave))
        b "save.canRetry" (ind |> Option.exists (fun i -> i.CanRetry))
        b "save.canReconnect" (ind |> Option.exists (fun i -> i.CanReconnect))
        b "save.canCheckAgain" (ind |> Option.exists (fun i -> i.CanCheckAgain))
        b "save.isSaved" (match session with Some { Save = Saved _ } -> true | _ -> false)
        b "save.isOperation" (match session with Some { Save = (UnknownOutcome | SaveFailed _) } -> true | _ -> false)
        b "save.isConflict" (ind |> Option.exists (fun i -> i.InConflict))
        b "save.isPlain" (match session with Some { Save = (UnknownOutcome | SaveFailed _ | Conflict) } -> false | _ -> true)
        t "save.fault" (session |> Option.bind (fun s -> s.Fault) |> Option.defaultValue "")
        "observations", VList(entities inv "observation" m.Selected)
        "hypotheses", VList(entities inv "hypothesis" m.Selected)
        "evidence", VList(entities inv "evidence" m.Selected)
        "relationships", VList(entities inv "relationship" m.Selected)
        "unknowns", VList(entities inv "unknown" m.Selected)
        "causes", VList(entities inv "cause" m.Selected)
        "conclusions", VList(entities inv "conclusion" m.Selected)
        "hypothesisOptions", VList(options inv "hypothesis")
        "evidenceOptions", VList(options inv "evidence")
        "timeline", VList(session |> Option.map events |> Option.defaultValue [])
        "transitions", VList transitions
        "lifecycleActions", VList lifecycle
        b "hasSelection" selectedEntity.IsSome
        t "sel.kind" (selectedEntity |> Option.map (fun e -> humanize e.Kind) |> Option.defaultValue "")
        t "sel.label" (selectedEntity |> Option.map entityLabel |> Option.defaultValue "")
        t "sel.state" (selectedEntity |> Option.bind (fun e -> e.State) |> Option.map humanize |> Option.defaultValue "No lifecycle")
        t "sel.recorded" (selectedEntity |> Option.map (fun e -> sprintf "Recorded in version %d" e.RecordedVersion) |> Option.defaultValue "")
        b "sel.hasTransitions" (not transitions.IsEmpty)
    ]
    @ (draftKeys |> List.map (fun k -> t ("d." + k) (draft m k)))
    @ [ // Conflict review (PERSIST-012)
        b "conflict.reviewing" (conflict |> Option.exists (fun c -> c.Reviewing))
        b "conflict.confirmDiscard" (conflict |> Option.exists (fun c -> c.ConfirmDiscard))
        b "conflict.theirsReady" theirs.IsSome
        t "conflict.mine" (summary inv + sprintf " · %d changes only in this tab" (session |> Option.map (fun s -> s.Unsaved.Length) |> Option.defaultValue 0))
        t "conflict.theirs" (match theirs, conflict with
                             | Some v, Some c -> summary (Some v.Investigation) + " · commit " + c.Head.Substring(0, 7)
                             | None, Some { TheirsError = Some e } -> "The GitHub version could not be read: " + e
                             | _ -> "Reading the GitHub version")
        "mineChanges", VList(session |> Option.map (fun s -> s.Unsaved |> List.mapi (fun i (_, e) -> [ "n", VText(string (i + 1)); "summary", VText(humanize e.EventType + ": " + eventSummary e) ])) |> Option.defaultValue [])
        "refused", VList(session |> Option.map (fun s -> s.Refused |> List.mapi (fun i (t, r) -> [ "n", VText(string i); "summary", VText(humanize t + ": " + r) ])) |> Option.defaultValue [])
        b "hasRefused" (session |> Option.exists (fun s -> not s.Refused.IsEmpty))
    ]
