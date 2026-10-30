/// Causa v1.0.0 investigation file sets: manifest.json + events.ndjson + snapshot.json.
/// Reading verifies everything a reader must (contract §4-§8); writing builds all three together.
module Causa.Domain.FileSet

open System
open Causa.Domain.Json
open Causa.Domain.Engine

type Files = { Manifest: string; Events: string; Snapshot: string }

type Verified =
    { Investigation: Investigation
      Events: Envelope list
      Files: Files }

let envelopeFields =
    [ "formatVersion"; "eventId"; "investigationId"; "aggregateVersion"; "eventType"; "occurredAt"; "actor"; "semanticVersion"; "correlationId"; "payload" ]

let private uuidField name json =
    str name json |> Result.bind (fun s -> if Text.isUuid s then Ok s else Error(name + " must be a lowercase UUID"))

let private timeField name json =
    str name json |> Result.bind (fun s -> if Text.isTimestamp s then Ok s else Error(name + " must be a UTC timestamp with milliseconds"))

let private checkActor json =
    match field "actor" json with
    | Some actor ->
        result {
            do! onlyKeys [ "actorId"; "kind"; "displayName" ] actor
            let! _ = nonEmpty "actorId" actor
            let! kind = str "kind" actor
            do! Result.require (List.contains kind Registry.Enums.actorKind) "actor.kind is not allowed"
            do! (match field "displayName" actor with None | Some(JString _) -> Ok() | Some _ -> Error "actor.displayName must be a string")
            return actor
        }
    | None -> Error "actor is required"

let decodeEnvelope (json: Json) : Result<Envelope, string> =
    result {
        do! onlyKeys envelopeFields json
        let! fv = str "formatVersion" json
        do! Result.require (fv = "1.0.0") "formatVersion must be 1.0.0"
        let! eventId = uuidField "eventId" json
        let! investigationId = uuidField "investigationId" json
        let! version = int64Field "aggregateVersion" json
        let! eventType = str "eventType" json
        let! occurredAt = timeField "occurredAt" json
        let! actor = checkActor json
        let! sv = str "semanticVersion" json
        do! Result.require (sv = "1.0.0") "semanticVersion must be 1.0.0"
        let! _ = uuidField "correlationId" json
        let! payload = field "payload" json |> Option.map Ok |> Option.defaultValue (Error "payload is required")
        return
            { EventId = eventId; InvestigationId = investigationId; AggregateVersion = version; EventType = eventType
              OccurredAt = occurredAt; Actor = actor; Payload = payload; Raw = json }
    }

/// Splits events.ndjson: UTF-8, LF only, one object per line, terminal LF.
let parseEvents (text: string) : Result<Envelope list, string> =
    if text.Length = 0 then Error "events.ndjson is empty"
    elif text.Contains '\r' then Error "events.ndjson must use LF line endings"
    elif not (text.EndsWith "\n") then Error "events.ndjson must end with LF"
    else
        text.Substring(0, text.Length - 1).Split('\n')
        |> List.ofArray
        |> List.mapi (fun i line -> i + 1, line)
        |> Result.traverse (fun (n, line) ->
            parse line |> Result.bind decodeEnvelope |> Result.mapError (sprintf "events.ndjson line %d: %s" n))

let entityJson (e: Entity) =
    JObject(("id", JString e.Id) :: e.Fields @ (match e.State with Some v -> [ stateKey e.Kind, JString v ] | None -> []))

/// The snapshot `state` object: the projection of the replayed history (causa-data v1.0.0 §6).
let stateJson (inv: Investigation) : Json =
    let arrayOf kind =
        inv.Entities
        |> Map.toList
        |> List.map snd
        |> List.filter (fun e -> e.Kind = kind)
        |> List.sortWith (fun a b -> String.CompareOrdinal(a.Id, b.Id))
        |> List.map entityJson
        |> JArray
    JObject(
        [ "investigationId", JString inv.Id; "title", JString inv.Title; "lifecycle", JString inv.Lifecycle
          "version", jint inv.Version; "createdAt", JString inv.CreatedAt ]
        @ (collections |> List.map (fun (kind, name) -> name, arrayOf kind))
        @ [ "proposals", JArray [] ])

let snapshotJson (inv: Investigation) : Json =
    JObject [
        "formatVersion", JString "1.0.0"
        "investigationId", JString inv.Id
        "aggregateVersion", jint inv.Version
        "projectionVersion", jint 1L
        "semanticVersion", JString "1.0.0"
        "generatedAt", JString inv.UpdatedAt
        "state", stateJson inv
    ]

/// Where two JSON values differ, as paths and kinds only (never values).
let rec diffPaths (path: string) (expected: Json) (actual: Json) : string list =
    match expected, actual with
    | JObject es, JObject acts ->
        let ek, ak = List.map fst es |> Set.ofList, List.map fst acts |> Set.ofList
        [ for k in Set.difference ek ak -> sprintf "%s.%s: missing in replay" path k
          for k in Set.difference ak ek -> sprintf "%s.%s: extra in replay" path k
          for (k, v) in es do
              match acts |> List.tryFind (fst >> (=) k) with
              | Some(_, w) -> yield! diffPaths (path + "." + k) v w
              | None -> () ]
    | JArray xs, JArray ys when xs.Length <> ys.Length -> [ sprintf "%s: %d items expected, %d in replay" path xs.Length ys.Length ]
    | JArray xs, JArray ys -> List.zip xs ys |> List.mapi (fun i (x, y) -> diffPaths (sprintf "%s[%d]" path i) x y) |> List.concat
    | a, b when canonical a = canonical b -> []
    | JString _, JString _ | JNumber _, JNumber _ | JBool _, JBool _ -> [ path + ": value differs" ]
    | _ -> [ path + ": type differs" ]

let manifestJson (inv: Investigation) (eventsSha: string) (snapshotSha: string) : Json =
    JObject [
        "formatVersion", JString "1.0.0"
        "investigationId", JString inv.Id
        "schemaVersion", JString "1.0.0"
        "semanticVersion", JString "1.0.0"
        "aggregateVersion", jint inv.Version
        "eventCount", jint inv.Version
        "eventsSha256", JString eventsSha
        "snapshotVersion", jint 1L
        "snapshotSha256", JString snapshotSha
        "domainPacks", JArray []
        "createdAt", JString inv.CreatedAt
        "updatedAt", JString inv.UpdatedAt
    ]

let manifestFields =
    [ "formatVersion"; "investigationId"; "schemaVersion"; "semanticVersion"; "aggregateVersion"; "eventCount"; "eventsSha256"
      "snapshotVersion"; "snapshotSha256"; "domainPacks"; "createdAt"; "updatedAt" ]

/// Fields whose values the v1.0.0 contract fixes: any other value is incompatible data (causa-data v1.0.0 §4, §6).
let private fixedValues (expected: (string * Json) list) (json: Json) =
    match expected |> List.filter (fun (k, v) -> field k json |> Option.map canonical <> Some(canonical v)) with
    | [] -> Ok()
    | wrong -> Error("unsupported value (incompatible data) for " + String.Join(", ", wrong |> List.map fst))

let private checkSnapshot (inv: Investigation) (snapshot: Json) =
    result {
        do! exactKeys [ "formatVersion"; "investigationId"; "aggregateVersion"; "projectionVersion"; "semanticVersion"; "generatedAt"; "state" ] snapshot
        let! id = str "investigationId" snapshot
        do! Result.require (id = inv.Id) "snapshot belongs to another investigation"
        let! v = int64Field "aggregateVersion" snapshot
        do! Result.require (v = inv.Version) "snapshot aggregateVersion does not match the replayed history"
        do! fixedValues [ "formatVersion", JString "1.0.0"; "semanticVersion", JString "1.0.0"; "projectionVersion", jint 1L ] snapshot
        let! state = field "state" snapshot |> Option.map Ok |> Option.defaultValue (Error "snapshot state is required")
        let! lifecycle = str "lifecycle" state
        do! Result.require (lifecycle = inv.Lifecycle) "snapshot lifecycle does not match the replayed history"
        let! title = str "title" state
        do! Result.require (title = inv.Title) "snapshot title does not match the replayed history"
        // The whole state must be what replaying the history produces (causa-data v1.0.0 §11).
        match diffPaths "state" state (stateJson inv) with
        | [] -> return ()
        | first :: rest -> return! Error(sprintf "state does not match the replayed history (%s%s)" first (if rest.IsEmpty then "" else sprintf " and %d more" rest.Length))
    }

/// Verifies a file set read from one commit, as a reader would (causa-data v1.0.0 §11).
let verify (expectedId: string) (files: Files) : Result<Verified, string> =
    result {
        let! manifest = parse files.Manifest |> Result.mapError ((+) "manifest.json: ")
        do! exactKeys manifestFields manifest |> Result.mapError ((+) "manifest.json: ")
        let! fv = str "formatVersion" manifest
        do! Result.require (fv = "1.0.0") "manifest.json: unsupported formatVersion (incompatible data)"
        do! fixedValues [ "schemaVersion", JString "1.0.0"; "semanticVersion", JString "1.0.0"; "snapshotVersion", jint 1L; "domainPacks", JArray [] ] manifest
            |> Result.mapError ((+) "manifest.json: ")
        let! mid = str "investigationId" manifest
        do! Result.require (mid = expectedId) "manifest.json: investigationId does not match its directory"
        let! events = parseEvents files.Events
        let! inv = replay events
        let! count = int64Field "eventCount" manifest
        let! version = int64Field "aggregateVersion" manifest
        do! Result.require (count = inv.Version && version = inv.Version) "manifest.json: version does not match the event history"
        let! eventsSha = str "eventsSha256" manifest
        do! Result.require (eventsSha = sha256Hex (utf8 files.Events)) "manifest.json: eventsSha256 does not match events.ndjson"
        do! Result.require (inv.Id = expectedId) "events belong to another investigation"
        let! snapshot = parse files.Snapshot |> Result.mapError ((+) "snapshot.json: ")
        let! snapshotSha = str "snapshotSha256" manifest
        do! Result.require (snapshotSha = canonicalSha256 snapshot) "manifest.json: snapshotSha256 does not match snapshot.json"
        do! checkSnapshot inv snapshot |> Result.mapError ((+) "snapshot.json: ")
        return { Investigation = inv; Events = events; Files = files }
    }

let eventLine (env: Envelope) = compact env.Raw + "\n"

/// The file set after appending `added` (already decided) to a verified base.
/// Existing event lines are kept byte for byte.
let build (baseEvents: string) (inv: Investigation) (added: Envelope list) : Files =
    let events = baseEvents + String.Join("", added |> List.map eventLine)
    let snapshot = snapshotJson inv
    let manifest = manifestJson inv (sha256Hex (utf8 events)) (canonicalSha256 snapshot)
    { Manifest = canonical manifest + "\n"; Events = events; Snapshot = canonical snapshot + "\n" }

let investigationPrefix (workspace: string) (id: string) = workspace.TrimEnd('/') + "/investigations/" + id + "/"
