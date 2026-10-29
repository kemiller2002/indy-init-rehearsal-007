/// One save request as a pure process (PERSIST-006 to PERSIST-016).
/// Each GitHub reply yields the next request, a wait, or an outcome, with the save-state inputs it implies.
module Causa.Persistence.SaveProcess

open Causa.Domain.FileSet
open Causa.Persistence.GitHub

let maxMutations = 4
let maxAutomaticReadsPerOutcome = 3
let maxAutomaticReadsPerSave = 4
let readDelaysMs = [| 2000; 4000; 8000 |]

let fileNames = [ "manifest.json"; "events.ndjson"; "snapshot.json" ]

let private pick (f: Files) name =
    match name with
    | "manifest.json" -> f.Manifest
    | "events.ndjson" -> f.Events
    | _ -> f.Snapshot

let private assemble (got: Map<string, string>) =
    { Manifest = got.["manifest.json"]; Events = got.["events.ndjson"]; Snapshot = got.["snapshot.json"] }

type Phase =
    | Committing
    | ReadingBack of commit: string * got: Map<string, string>
    | CheckingHead
    | ComparingFiles of newHead: string * got: Map<string, string>
    | Reconciling
    | WaitingToReconcile of automatic: bool

type Save =
    { Connection: Connection
      InvestigationId: string
      Prefix: string
      BaseCommit: string
      BaseFiles: Files option
      Files: Files
      SentEvents: int
      Session: string
      Sequence: int
      Expected: string
      Mutations: int
      ReadsThisOutcome: int
      AutomaticReads: int
      Phase: Phase
      Headline: string }

type Outcome =
    | Done of Confirmation
    | ConflictAt of head: string
    | Stopped

type Next =
    | Send of Save * Request * SaveInput list
    | Wait of Save * delayMs: int * SaveInput list
    | AwaitUser of Save * SaveInput list
    | Finish of Outcome * SaveInput list

let marker (s: Save) = sprintf "%s-%d" s.Session s.Sequence

/// Paths a commit may change: only the investigation's own prefix (causa-data v1.0.0 §9.4.4).
let commitFiles (s: Save) = fileNames |> List.map (fun n -> s.Prefix + n, pick s.Files n)

let private insidePrefix (s: Save) =
    commitFiles s |> List.forall (fun (p, _) -> p.StartsWith s.Prefix && not (p.Contains "..") && p.Substring(s.Prefix.Length).IndexOf '/' < 0)

let private sendCommit (s: Save) (inputs: SaveInput list) : Next =
    if s.Mutations >= maxMutations then Finish(Stopped, inputs @ [ Failed RepositoryKeptChanging ])
    elif not (insidePrefix s) then Finish(Stopped, inputs @ [ Failed(Rejected "the commit would change files outside this investigation") ])
    else
        let s' = { s with Mutations = s.Mutations + 1; Sequence = s.Sequence + 1; Phase = Committing }
        Send(s', createCommit s.Connection s'.Expected s.Headline (marker s') (commitFiles s'), inputs @ [ MutationSent ])

let private readFile (s: Save) (commit: string) (got: Map<string, string>) =
    fileNames |> List.tryFind (fun n -> not (got.ContainsKey n)) |> Option.map (fun n -> file s.Connection (s.Prefix + n) commit)

/// Begins a save. `nextSequence` is the tab's write-marker sequence.
let start (connection: Connection) (investigationId: string) (prefix: string) (baseCommit: string) (baseFiles: Files option) (files: Files) (sentEvents: int) (session: string) (sequence: int) (headline: string) : Next =
    sendCommit
        { Connection = connection; InvestigationId = investigationId; Prefix = prefix; BaseCommit = baseCommit; BaseFiles = baseFiles; Files = files
          SentEvents = sentEvents; Session = session; Sequence = sequence; Expected = baseCommit
          Mutations = 0; ReadsThisOutcome = 0; AutomaticReads = 0; Phase = Committing; Headline = headline }
        []

let private scheduleReconcile (s: Save) (inputs: SaveInput list) : Next =
    if s.ReadsThisOutcome < maxAutomaticReadsPerOutcome && s.AutomaticReads < maxAutomaticReadsPerSave then
        Wait({ s with Phase = WaitingToReconcile true }, readDelaysMs.[s.ReadsThisOutcome], inputs)
    else AwaitUser({ s with Phase = WaitingToReconcile false }, inputs)

let private unknownOutcome (s: Save) = scheduleReconcile { s with ReadsThisOutcome = 0 } [ OutcomeLost ]

/// A reconciliation read: automatic (timer, reconnect) or requested by the user (Check again).
let reconcileNow (s: Save) (automatic: bool) (nonce: string) : Next option =
    match s.Phase with
    | WaitingToReconcile _ when automatic && s.AutomaticReads >= maxAutomaticReadsPerSave -> None
    | WaitingToReconcile _ ->
        let s' =
            { s with
                Phase = Reconciling
                ReadsThisOutcome = s.ReadsThisOutcome + 1
                AutomaticReads = if automatic then s.AutomaticReads + 1 else s.AutomaticReads }
        Some(Send(s', compare s.Connection s.Expected s.Connection.Branch nonce, []))
    | _ -> None

let private readFailure (s: Save) (answer: ReadAnswer<'a>) : Next =
    match answer with
    | ReadOffline -> Finish(Stopped, [ NotSent ])
    | ReadDenied true -> Finish(Stopped, [ Failed CredentialRefused ])
    | ReadDenied false -> Finish(Stopped, [ Failed PermissionDenied ])
    | ReadRateLimited -> Finish(Stopped, [ Failed RateLimited ])
    | ReadFailed e -> Finish(Stopped, [ Failed(Rejected("GitHub could not be read: " + e)) ])
    | Read _ | Missing -> Finish(Stopped, [ Failed(Rejected "unexpected read") ])

let private hasMarker (m: string) (message: string) =
    message.Split('\n') |> Array.exists (fun line -> line.Trim() = markerTrailer + ": " + m)

/// Advances the save with the reply to the request it last sent.
let onReply (s: Save) (reply: Reply) (now: string) (nonce: string) : Next =
    match s.Phase with
    | Committing ->
        match classifyCommit reply with
        | Committed oid ->
            let s' = { s with Phase = ReadingBack(oid, Map.empty) }
            Send(s', (readFile s' oid Map.empty).Value, [])
        | StaleHead -> Send({ s with Phase = CheckingHead }, head s.Connection nonce, [])
        | CredentialRejected -> Finish(Stopped, [ Failed CredentialRefused ])
        | PermissionRefused -> Finish(Stopped, [ Failed PermissionDenied ])
        | RateLimit -> Finish(Stopped, [ Failed RateLimited ])
        | ValidationRefused m -> Finish(Stopped, [ Failed(Rejected m) ])
        | NetworkNotSent -> Finish(Stopped, [ NotSent ])
        | CommitAnswer.OutcomeUnknown -> unknownOutcome s
    | ReadingBack(commit, got) ->
        match classifyRead GitHub.decodeText reply with
        | Read text ->
            let got' = got.Add(fileNames.[got.Count], text)
            match readFile s commit got' with
            | Some request -> Send({ s with Phase = ReadingBack(commit, got') }, request, [])
            | None ->
                match Confirmation.ofReadBack commit s.Files (assemble got') now with
                | Ok c -> Finish(Done c, [])
                | Error _ -> Finish(Stopped, [ Failed ReadBackMismatch ])
        | _ -> unknownOutcome s
    | CheckingHead ->
        match classifyRead decodeHead reply with
        | Read h when h = s.Expected ->
            // GitHub refused a stale head but the head did not move: treat as unknown, never as a conflict.
            unknownOutcome s
        | Read h ->
            let s' = { s with Phase = ComparingFiles(h, Map.empty) }
            Send(s', (readFile s' h Map.empty).Value, [])
        | other -> readFailure s other
    | ComparingFiles(newHead, got) ->
        match classifyRead GitHub.decodeText reply with
        | Read text ->
            let got' = got.Add(fileNames.[got.Count], text)
            match readFile s newHead got' with
            | Some request -> Send({ s with Phase = ComparingFiles(newHead, got') }, request, [])
            | None when s.BaseFiles = Some(assemble got') -> sendCommit { s with Expected = newHead } [ StaleUnrelated ]
            | None -> Finish(ConflictAt newHead, [ StaleConflict ])
        | Missing when s.BaseFiles.IsNone && got.IsEmpty ->
            // A new investigation whose files are still absent: the branch moved for unrelated changes.
            sendCommit { s with Expected = newHead } [ StaleUnrelated ]
        | Missing -> Finish(ConflictAt newHead, [ StaleConflict ])
        | other -> readFailure s other
    | Reconciling ->
        match classifyRead decodeCompare reply with
        | Read cmp when cmp.Status = "ahead" || cmp.Status = "identical" ->
            match cmp.Commits |> List.tryFind (snd >> hasMarker (marker s)) with
            | Some(sha, _) ->
                let s' = { s with Phase = ReadingBack(sha, Map.empty) }
                Send(s', (readFile s' sha Map.empty).Value, [ StaleUnrelated ])
            | None when cmp.Status = "identical" -> sendCommit s [ NothingWritten ]
            | None ->
                let h = cmp.Commits |> List.last |> fst
                let s' = { s with Phase = ComparingFiles(h, Map.empty) }
                Send(s', (readFile s' h Map.empty).Value, [ StaleUnrelated ])
        | Read _ | Missing -> Finish(Stopped, [ Failed HistoryRewritten ])
        | _ -> scheduleReconcile s []
    | WaitingToReconcile _ -> AwaitUser(s, [])
