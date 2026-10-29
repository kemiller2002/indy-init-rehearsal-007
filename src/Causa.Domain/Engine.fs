/// The one domain decision path: envelopes are validated and applied by `evolve`;
/// commands become envelopes only if `evolve` accepts them (`decide`).
module Causa.Domain.Engine

open Causa.Domain.Json
open Causa.Domain.Registry

type Envelope =
    { EventId: string
      InvestigationId: string
      AggregateVersion: int64
      EventType: string
      OccurredAt: string
      RecordedAt: string
      Payload: Json
      /// The envelope exactly as read or written, so history is never re-serialized.
      Raw: Json }

type Entity =
    { Id: string
      Kind: string
      State: string option
      PriorState: string option
      Fields: (string * Json) list
      RecordedVersion: int64 }

type Investigation =
    { Id: string
      Title: string
      OriginalProblem: string
      Lifecycle: string
      Version: int64
      CreatedAt: string
      UpdatedAt: string
      Entities: Map<string, Entity>
      EventIds: Set<string> }

/// Snapshot array name for each entity kind (causa-data v1.0.0 §6).
let collections =
    [ "observation", "observations"; "hypothesis", "hypotheses"; "evidence", "evidence"; "relationship", "relationships"
      "unknown", "unknowns"; "assumption", "assumptions"; "contradiction", "contradictions"; "prediction", "predictions"
      "action", "actions"; "finding", "findings"; "cause", "causes"; "correctiveAction", "correctiveActions"
      "criterion", "criteria"; "verification", "verifications"; "conclusion", "conclusions" ]

type Reference =
    | One of field: string * kind: string
    | Many of field: string * kind: string
    | MemberHypotheses

type Creation = { Kind: string; IdField: string; Initial: Json -> string option; References: Reference list }

let private fixedState s = fun (_: Json) -> Some s
let private noState = fun (_: Json) -> None

let creations : Map<string, Creation> =
    Map.ofList [
        "ObservationRecorded", { Kind = "observation"; IdField = "observationId"; Initial = noState; References = [] }
        "HypothesisProposed", { Kind = "hypothesis"; IdField = "hypothesisId"; Initial = fixedState "proposed"; References = [] }
        "EvidenceAdmitted",
        { Kind = "evidence"; IdField = "evidenceId"; References = []
          Initial = fun p -> match field "verification" p with Some(JString v) -> Some v | _ -> None }
        "EvidenceRelatedToHypothesis",
        { Kind = "relationship"; IdField = "relationshipId"; Initial = noState
          References = [ One("evidenceId", "evidence"); One("hypothesisId", "hypothesis") ] }
        "UnknownIdentified", { Kind = "unknown"; IdField = "unknownId"; Initial = fixedState "identified"; References = [] }
        "AssumptionRecorded", { Kind = "assumption"; IdField = "assumptionId"; Initial = fixedState "explicitUnverified"; References = [] }
        "ContradictionDetected",
        { Kind = "contradiction"; IdField = "contradictionId"; Initial = fixedState "detected"; References = [ Many("evidenceIds", "evidence") ] }
        "PredictionRecorded", { Kind = "prediction"; IdField = "predictionId"; Initial = noState; References = [ One("hypothesisId", "hypothesis") ] }
        "InvestigationActionProposed",
        { Kind = "action"; IdField = "actionId"; Initial = fixedState "proposed"; References = [ Many("targetHypothesisIds", "hypothesis") ] }
        "FindingRecorded", { Kind = "finding"; IdField = "findingId"; Initial = noState; References = [ One("actionId", "action") ] }
        "CausalAssertionProposed", { Kind = "cause"; IdField = "causeId"; Initial = fixedState "proposed"; References = [ MemberHypotheses ] }
        "CorrectiveActionProposed",
        { Kind = "correctiveAction"; IdField = "correctiveActionId"; Initial = fixedState "proposed"; References = [ Many("targetCauseIds", "cause") ] }
        "VerificationCriterionRecorded",
        { Kind = "criterion"; IdField = "criterionId"; Initial = noState; References = [ One("correctiveActionId", "correctiveAction") ] }
        "VerificationRecorded",
        { Kind = "verification"; IdField = "verificationId"; Initial = noState
          References = [ One("criterionId", "criterion"); Many("evidenceIds", "evidence") ] }
        "ConclusionRecorded", { Kind = "conclusion"; IdField = "conclusionId"; Initial = noState; References = [ Many("causeIds", "cause") ] }
    ]

let private pairs (froms: string list) (tos: string list) = [ for f in froms do for t in tos -> f, t ]

/// Legal (from, to) pairs per lifecycle (docs/STATE-TRANSITION-TABLES.md §2, 4-11).
let transitions : Map<string, Set<string * string>> =
    let open4 = [ "proposed"; "plausible"; "supported"; "weakened" ]
    Map.ofList [
        "hypothesis",
        set (
            [ "proposed", "plausible"; "plausible", "supported"; "falsified", "plausible"; "weakened", "plausible"; "weakened", "supported" ]
            @ pairs [ "plausible"; "supported" ] [ "weakened" ]
            @ pairs open4 [ "falsified"; "superseded" ])
        "evidence", set [ for f in Enums.evidenceVerification do for t in Enums.evidenceVerification do if f <> t then yield f, t ]
        "unknown", set ([ "identified", "investigating" ] @ pairs [ "identified"; "investigating" ] [ "resolved"; "acceptedResidual"; "superseded" ])
        "contradiction",
        set (
            [ "detected", "triaged" ]
            @ pairs [ "detected"; "triaged" ] [ "investigating" ]
            @ pairs [ "detected"; "triaged"; "investigating" ] [ "resolved"; "acceptedResidual"; "superseded" ])
        "action",
        set (
            [ "proposed", "approved"; "proposed", "rejected"; "approved", "inProgress"; "inProgress", "completed"; "completed", "interpreted" ]
            @ pairs [ "approved"; "inProgress" ] [ "blocked"; "cancelled" ]
            @ pairs [ "blocked" ] [ "approved"; "inProgress" ])
        "cause", set [ "proposed", "confirmed"; "confirmed", "reopened"; "proposed", "superseded"; "reopened", "superseded"; "reopened", "confirmed" ]
        "correctiveAction",
        set (
            [ "proposed", "approved"; "approved", "inProgress"; "inProgress", "implemented"; "implemented", "awaitingVerification"
              "awaitingVerification", "verifiedEffective"; "awaitingVerification", "verificationFailed"
              "verificationFailed", "reworkRequired"; "reworkRequired", "inProgress" ]
            @ pairs [ "proposed"; "approved" ] [ "cancelled" ])
    ]

/// State-change events: entity kind and its id field.
let stateChanges : Map<string, string * string> =
    Map.ofList [
        "HypothesisStateChanged", ("hypothesis", "hypothesisId")
        "EvidenceVerificationChanged", ("evidence", "evidenceId")
        "UnknownStateChanged", ("unknown", "unknownId")
        "ContradictionStateChanged", ("contradiction", "contradictionId")
        "InvestigationActionStateChanged", ("action", "actionId")
        "CausalAssertionStateChanged", ("cause", "causeId")
        "CorrectiveActionStateChanged", ("correctiveAction", "correctiveActionId")
    ]

/// Investigation lifecycle events: legal source states and the target state (§1).
let lifecycleEvents : Map<string, string list * string> =
    let nonArchived = Enums.lifecycle |> List.filter ((<>) "archived")
    Map.ofList [
        "InvestigationActivated", ([ "draft"; "awaitingEvidence"; "awaitingVerification"; "archived" ], "active")
        "InvestigationMarkedAwaitingEvidence", ([ "active" ], "awaitingEvidence")
        "InvestigationMarkedAwaitingVerification", ([ "active" ], "awaitingVerification")
        "InvestigationResolved", ([ "active"; "awaitingVerification" ], "resolved")
        "InvestigationClosed", ([ "resolved" ], "closed")
        "InvestigationReopened", ([ "resolved"; "closed" ], "active")
        "InvestigationArchived", (nonArchived, "archived")
    ]

let private text name payload = match field name payload with Some(JString s) -> s | _ -> ""

let private requireEntity (inv: Investigation) (kind: string) (id: string) =
    match Map.tryFind id inv.Entities with
    | Some e when e.Kind = kind -> Ok e
    | Some _ -> Error(sprintf "%s is not a %s" id kind)
    | None -> Error(sprintf "unknown %s %s" kind id)

let private checkReference inv payload reference =
    match reference with
    | One(name, kind) -> requireEntity inv kind (text name payload) |> Result.map ignore
    | Many(name, kind) ->
        match field name payload with
        | Some(JArray ids) -> ids |> Result.traverse (function JString id -> requireEntity inv kind id | _ -> Error "bad id") |> Result.map ignore
        | _ -> Error(name + " missing")
    | MemberHypotheses ->
        match field "members" payload with
        | Some(JArray ms) -> ms |> Result.traverse (fun m -> requireEntity inv "hypothesis" (text "hypothesisId" m)) |> Result.map ignore
        | _ -> Error "members missing"

let private writable (inv: Investigation) =
    Result.require (inv.Lifecycle <> "archived" && inv.Lifecycle <> "closed") (sprintf "the investigation is %s" inv.Lifecycle)

let private applyCreation inv (env: Envelope) (c: Creation) =
    result {
        do! writable inv
        let id = text c.IdField env.Payload
        do! Result.require (not (Map.containsKey id inv.Entities)) (sprintf "%s %s already exists" c.Kind id)
        do! c.References |> Result.traverse (checkReference inv env.Payload) |> Result.map ignore
        let fields = match env.Payload with JObject fs -> fs |> List.filter (fst >> (<>) c.IdField) | _ -> []
        let entity = { Id = id; Kind = c.Kind; State = c.Initial env.Payload; PriorState = None; Fields = fields; RecordedVersion = env.AggregateVersion }
        return { inv with Entities = Map.add id entity inv.Entities }
    }

let private applyStateChange inv (env: Envelope) (kind, idField) =
    result {
        do! writable inv
        let! entity = requireEntity inv kind (text idField env.Payload)
        let fromState, toState = text "from" env.Payload, text "to" env.Payload
        do! Result.require (entity.State = Some fromState) (sprintf "%s %s is %s, not %s" kind entity.Id (defaultArg entity.State "-") fromState)
        do! Result.require (Set.contains (fromState, toState) transitions.[kind]) (sprintf "%s cannot move from %s to %s" kind fromState toState)
        do! Result.require (not (kind = "action" && fromState = "blocked") || entity.PriorState = Some toState) "a blocked action returns only to its prior state"
        do! (match field "evidenceIds" env.Payload with
             | Some(JArray ids) -> ids |> Result.traverse (function JString id -> requireEntity inv "evidence" id | _ -> Error "bad id") |> Result.map ignore
             | _ -> Ok())
        let updated = { entity with State = Some toState; PriorState = Some fromState }
        return { inv with Entities = Map.add entity.Id updated inv.Entities }
    }

let private applyLifecycle inv (env: Envelope) (froms: string list, target: string) =
    result {
        do! Result.require (List.contains inv.Lifecycle froms) (sprintf "the investigation cannot move from %s to %s" inv.Lifecycle target)
        do! (if env.EventType = "InvestigationResolved" then requireEntity inv "conclusion" (text "conclusionId" env.Payload) |> Result.map ignore else Ok())
        return { inv with Lifecycle = target }
    }

let private applyBody (inv: Investigation) (env: Envelope) : Result<Investigation, string> =
    match env.EventType with
    | "InvestigationStarted" -> Error "the investigation has already started"
    | "InvestigationRenamed" -> writable inv |> Result.map (fun () -> { inv with Title = text "title" env.Payload })
    | t when creations.ContainsKey t -> applyCreation inv env creations.[t]
    | t when stateChanges.ContainsKey t -> applyStateChange inv env stateChanges.[t]
    | t when lifecycleEvents.ContainsKey t -> applyLifecycle inv env lifecycleEvents.[t]
    | t -> Error("unknown event type " + t)

/// Applies one envelope to the state (None before the first event). Fails closed.
let evolve (state: Investigation option) (env: Envelope) : Result<Investigation, string> =
    result {
        do! validatePayload env.EventType env.Payload
        match state with
        | None ->
            do! Result.require (env.EventType = "InvestigationStarted") "history must begin with InvestigationStarted"
            do! Result.require (env.AggregateVersion = 1L) "the first event must have aggregateVersion 1"
            return
                { Id = env.InvestigationId
                  Title = text "title" env.Payload
                  OriginalProblem = text "originalProblem" env.Payload
                  Lifecycle = "draft"
                  Version = 1L
                  CreatedAt = env.OccurredAt
                  UpdatedAt = env.RecordedAt
                  Entities = Map.empty
                  EventIds = Set.singleton env.EventId }
        | Some inv ->
            do! Result.require (env.InvestigationId = inv.Id) "event belongs to another investigation"
            do! Result.require (env.AggregateVersion = inv.Version + 1L) (sprintf "aggregateVersion %d does not follow %d" env.AggregateVersion inv.Version)
            do! Result.require (not (inv.EventIds.Contains env.EventId)) ("duplicate eventId " + env.EventId)
            let! next = applyBody inv env
            return { next with Version = env.AggregateVersion; UpdatedAt = env.RecordedAt; EventIds = next.EventIds.Add env.EventId }
    }
    |> Result.mapError (fun e -> sprintf "event %d (%s): %s" env.AggregateVersion env.EventType e)

let replay (events: Envelope list) : Result<Investigation, string> =
    match events with
    | [] -> Error "the history is empty"
    | first :: rest -> evolve None first |> Result.bind (fun s -> Result.foldM (fun inv e -> evolve (Some inv) e) s rest)

// ---- Commands ----

type Actor = { ActorId: string; DisplayName: string option }

/// A user intention. For state-change events `Payload` omits `from`; `decide` reads it from the state,
/// so re-deciding a command on a newer version (conflict reapply) uses that version's state.
type Command = { EventType: string; Payload: Json }

type Context = { NewId: unit -> string; Now: unit -> string; Actor: Actor }

let private entityIdField (cmd: Command) = stateChanges |> Map.tryFind cmd.EventType |> Option.map snd

let private withFrom (state: Investigation option) (cmd: Command) : Result<Json, string> =
    match entityIdField cmd, state, cmd.Payload with
    | Some idField, Some inv, JObject fields ->
        let id = text idField cmd.Payload
        match Map.tryFind id inv.Entities |> Option.bind (fun e -> e.State) with
        | Some current ->
            let rest = fields |> List.filter (fst >> (<>) "from")
            Ok(JObject(rest |> List.collect (fun (k, v) -> if k = idField then [ k, v; "from", JString current ] else [ k, v ])))
        | None -> Error(sprintf "unknown %s" id)
    | _ -> Ok cmd.Payload

let envelopeJson (env: Envelope) (ctx: Context) (correlationId: string) =
    JObject [
        "formatVersion", JString "1.0.0"
        "eventId", JString env.EventId
        "investigationId", JString env.InvestigationId
        "aggregateVersion", jint env.AggregateVersion
        "eventType", JString env.EventType
        "occurredAt", JString env.OccurredAt
        "recordedAt", JString env.RecordedAt
        "actor", JObject [ "actorId", JString ctx.Actor.ActorId; "kind", JString "human"; "displayName", jstrOpt ctx.Actor.DisplayName ]
        "policyRefs", JArray []
        "semanticVersion", JString "1.0.0"
        "correlationId", JString correlationId
        "causationId", JNull
        "payload", env.Payload
        "provenance", JObject [ "source", JString "causa-app"; "sourceRef", JNull; "recordedBy", JString "causa-app" ]
    ]

/// Turns a command into the envelope it records, or the engine's refusal reason.
let decide (ctx: Context) (investigationId: string) (state: Investigation option) (cmd: Command) : Result<Envelope * Investigation, string> =
    result {
        let! payload = withFrom state cmd
        let now = ctx.Now()
        let env0 =
            { EventId = ctx.NewId()
              InvestigationId = investigationId
              AggregateVersion = (state |> Option.map (fun s -> s.Version) |> Option.defaultValue 0L) + 1L
              EventType = cmd.EventType
              OccurredAt = now
              RecordedAt = now
              Payload = payload
              Raw = JNull }
        let env = { env0 with Raw = envelopeJson env0 ctx (ctx.NewId()) }
        let! next = evolve state env
        return env, next
    }

/// Target states a command could move an entity to now, found by asking `evolve` (never re-derived).
let legalTargets (ctx: Context) (inv: Investigation) (entity: Entity) : (string * string) list =
    match stateChanges |> Map.toList |> List.tryFind (fun (_, (kind, _)) -> kind = entity.Kind) with
    | None -> []
    | Some(eventType, (kind, idField)) ->
        let values =
            match kind with
            | "hypothesis" -> Enums.hypothesis | "evidence" -> Enums.evidenceVerification | "unknown" -> Enums.unknown
            | "contradiction" -> Enums.contradiction | "action" -> Enums.action | "cause" -> Enums.cause
            | _ -> Enums.corrective
        let reasonField = if kind = "unknown" || kind = "contradiction" then "basis" else "reason"
        values
        |> List.choose (fun target ->
            let extra = if kind = "hypothesis" then [ "evidenceIds", JArray [] ] else []
            let payload = JObject([ idField, JString entity.Id; "to", JString target; reasonField, JString "check" ] @ extra)
            match decide ctx inv.Id (Some inv) { EventType = eventType; Payload = payload } with
            | Ok _ -> Some(eventType, target)
            | Error _ -> None)

/// Lifecycle events the engine would accept now.
let legalLifecycle (ctx: Context) (inv: Investigation) : string list =
    lifecycleEvents
    |> Map.toList
    |> List.map fst
    |> List.filter (fun eventType ->
        let conclusion = inv.Entities |> Map.tryPick (fun id e -> if e.Kind = "conclusion" then Some id else None)
        let payload =
            match eventType, conclusion with
            | "InvestigationResolved", Some c -> Some(JObject [ "reason", JString "check"; "conclusionId", JString c ])
            | "InvestigationResolved", None -> None
            | _ -> Some(JObject [ "reason", JString "check" ])
        payload |> Option.exists (fun p -> Result.isOk (decide ctx inv.Id (Some inv) { EventType = eventType; Payload = p })))
