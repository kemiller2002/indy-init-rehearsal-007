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
    [ "formatVersion"; "eventId"; "investigationId"; "aggregateVersion"; "eventType"; "occurredAt"; "recordedAt"; "actor"
      "policyRefs"; "semanticVersion"; "correlationId"; "causationId"; "payload"; "provenance" ]

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
            do! Result.require (List.contains kind [ "human"; "agent"; "system"; "external"; "imported" ]) "actor.kind is not allowed"
            let! _ = strOrNull "displayName" actor
            return ()
        }
    | None -> Error "actor is required"

let private checkProvenance json =
    match field "provenance" json with
    | Some p ->
        result {
            do! onlyKeys [ "source"; "sourceRef"; "recordedBy" ] p
            let! _ = nonEmpty "source" p
            let! _ = strOrNull "sourceRef" p
            let! _ = nonEmpty "recordedBy" p
            return ()
        }
    | None -> Error "provenance is required"

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
        let! recordedAt = timeField "recordedAt" json
        do! checkActor json
        let! refs = strArray "policyRefs" json
        do! Result.require (refs = (refs |> List.distinct |> List.sort)) "policyRefs must be sorted and unique"
        let! sv = str "semanticVersion" json
        do! Result.require (sv = "1.0.0") "semanticVersion must be 1.0.0"
        let! _ = uuidField "correlationId" json
        let! causation = strOrNull "causationId" json
        do! Result.require (causation |> Option.forall Text.isUuid) "causationId must be a UUID or null"
        do! checkProvenance json
        let! payload = field "payload" json |> Option.map Ok |> Option.defaultValue (Error "payload is required")
        return
            { EventId = eventId; InvestigationId = investigationId; AggregateVersion = version; EventType = eventType
              OccurredAt = occurredAt; RecordedAt = recordedAt; Payload = payload; Raw = json }
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

let private entityJson (e: Entity) =
    JObject(("id", JString e.Id) :: e.Fields @ (match e.State with Some s -> [ "state", JString s ] | None -> []) @ [ "recordedVersion", jint e.RecordedVersion ])

/// The derived snapshot `state` object: a deterministic projection of the replayed state.
let stateJson (inv: Investigation) : Json =
    let arrayOf name =
        let kind = collections |> List.tryFind (snd >> (=) name) |> Option.map fst
        inv.Entities
        |> Map.toList
        |> List.map snd
        |> List.filter (fun e -> Some e.Kind = kind)
        |> List.sortWith (fun a b -> String.CompareOrdinal(a.Id, b.Id))
        |> List.map entityJson
        |> JArray
    JObject(
        [ "investigationId", JString inv.Id; "title", JString inv.Title; "lifecycle", JString inv.Lifecycle
          "version", jint inv.Version; "createdAt", JString inv.CreatedAt ]
        @ (collections |> List.map (fun (_, name) -> name, arrayOf name))
        @ [ "negativeKnowledge", JArray []; "proposals", JArray [] ])

let snapshotJson (inv: Investigation) (generatedAt: string) : Json =
    JObject [
        "formatVersion", JString "1.0.0"
        "investigationId", JString inv.Id
        "aggregateVersion", jint inv.Version
        "projectionVersion", jint 1L
        "semanticVersion", JString "1.0.0"
        "generatedAt", JString generatedAt
        "state", stateJson inv
    ]

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

let private checkSnapshot (inv: Investigation) (snapshot: Json) =
    result {
        do! onlyKeys [ "formatVersion"; "investigationId"; "aggregateVersion"; "projectionVersion"; "semanticVersion"; "generatedAt"; "state" ] snapshot
        let! id = str "investigationId" snapshot
        do! Result.require (id = inv.Id) "snapshot belongs to another investigation"
        let! v = int64Field "aggregateVersion" snapshot
        do! Result.require (v = inv.Version) "snapshot aggregateVersion does not match the replayed history"
        let! state = field "state" snapshot |> Option.map Ok |> Option.defaultValue (Error "snapshot state is required")
        let! lifecycle = str "lifecycle" state
        do! Result.require (lifecycle = inv.Lifecycle) "snapshot lifecycle does not match the replayed history"
        let! title = str "title" state
        do! Result.require (title = inv.Title) "snapshot title does not match the replayed history"
        return ()
    }

/// Verifies a file set read from one commit, as a reader would (causa-data v1.0.0 §11).
let verify (expectedId: string) (files: Files) : Result<Verified, string> =
    result {
        let! manifest = parse files.Manifest |> Result.mapError ((+) "manifest.json: ")
        do! onlyKeys manifestFields manifest |> Result.mapError ((+) "manifest.json: ")
        let! fv = str "formatVersion" manifest
        do! Result.require (fv = "1.0.0") "manifest.json: unsupported formatVersion (incompatible data)"
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
let build (baseEvents: string) (inv: Investigation) (added: Envelope list) (generatedAt: string) : Files =
    let events = baseEvents + String.Join("", added |> List.map eventLine)
    let snapshot = snapshotJson inv generatedAt
    let manifest = manifestJson inv (sha256Hex (utf8 events)) (canonicalSha256 snapshot)
    { Manifest = canonical manifest + "\n"; Events = events; Snapshot = canonical snapshot + "\n" }

let investigationPrefix (workspace: string) (id: string) = workspace.TrimEnd('/') + "/investigations/" + id + "/"
