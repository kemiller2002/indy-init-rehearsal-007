/// Opening an investigation: its three files read at one commit and verified (PERSIST-007, causa-data v1.0.0 §11).
/// Outside a save, this is the only way application code obtains a Confirmation.
module Causa.Persistence.Load

open Causa.Domain.FileSet
open Causa.Persistence.GitHub

let fileNames = [ "manifest.json"; "events.ndjson"; "snapshot.json" ]

/// A load in progress. Private, so the bytes it confirms are only the ones GitHub answered.
type Load = private { Connection: Connection; InvestigationId: string; Commit: string; Got: string list }

type Step =
    | Next of Load * Request
    | Loaded of Verified * Confirmation
    | Refused of reason: string
    | NotRead of ReadAnswer<string>

let commit (l: Load) = l.Commit

let private request (l: Load) =
    file l.Connection (investigationPrefix l.Connection.Workspace l.InvestigationId + List.item l.Got.Length fileNames) l.Commit

/// Begins reading the investigation's files at `commit`.
let start (connection: Connection) (investigationId: string) (commit: string) : Load * Request =
    let l = { Connection = connection; InvestigationId = investigationId; Commit = commit; Got = [] }
    l, request l

/// Advances the load with the reply to the request it last sent.
let onReply (l: Load) (reply: Reply) (now: string) : Step =
    match classifyRead decodeText reply with
    | Read text when l.Got.Length + 1 < fileNames.Length ->
        let l' = { l with Got = l.Got @ [ text ] }
        Next(l', request l')
    | Read text ->
        match l.Got @ [ text ] with
        | [ manifest; events; snapshot ] ->
            Confirmation.ofVerifiedRead l.Commit l.InvestigationId { Manifest = manifest; Events = events; Snapshot = snapshot } now
            |> function
                | Ok(v, c) -> Loaded(v, c)
                | Error e -> Refused e
        | _ -> Refused "unexpected number of files"
    | other -> NotRead other
