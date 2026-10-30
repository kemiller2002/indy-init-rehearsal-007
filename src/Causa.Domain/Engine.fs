/// The one domain decision path: envelopes are validated and applied by `evolve`;
/// commands become envelopes only if `evolve` accepts them (`decide`).
/// Entities keep the fields of their snapshot projection (causa-data v1.0.0), so the snapshot is the state.
module Causa.Domain.Engine

open Causa.Domain.Json
open Causa.Domain.Registry

type Envelope =
    { EventId: string
      InvestigationId: string
      AggregateVersion: int64
      EventType: string
      OccurredAt: string
      Actor: Json
      Payload: Json
      /// The envelope exactly as read or written, so history is never re-serialized.
      Raw: Json }

type Entity =
    { Id: string
      Kind: string
      /// The lifecycle value (snapshot key `state`, or `verification` for evidence).
      State: string option
      /// Every other snapshot field of the entity except `id`.
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
      "action", "actions"; "finding", "findings"; "negativeKnowledge", "negativeKnowledge"; "cause", "causes"
      "correctiveAction", "correctiveActions"; "criterion", "criteria"; "verification", "verifications"; "conclusion", "conclusions" ]

let stateKey (kind: string) = if kind = "evidence" then "verification" else "state"

type Reference =
    | One of field: string * kind: string
    | Many of field: string * kind: string
    | MemberHypotheses

type Creation =
    { Kind: string
      IdField: string
      Initial: string option
      /// Fields the projection adds at creation, from the envelope.
      Extra: Envelope -> (string * Json) list
      References: Reference list }

let private noExtra = fun (_: Envelope) -> []

let creations : Map<string, Creation> =
    let c kind idField initial extra refs = { Kind = kind; IdField = idField; Initial = initial; Extra = extra; References = refs }
    Map.ofList [
        "ObservationRecorded", c "observation" "observationId" None noExtra []
        "HypothesisRecorded", c "hypothesis" "hypothesisId" (Some "proposed") (fun _ -> [ "history", JArray [] ]) []
        "EvidenceAdmitted",
        c "evidence" "evidenceId" (Some "unverified") (fun e -> [ "admittedAtVersion", jint e.AggregateVersion; "supersededBy", JNull ]) []
        "EvidenceRelated", c "relationship" "relationshipId" None noExtra [ One("evidenceId", "evidence"); One("hypothesisId", "hypothesis") ]
        "PredictionRecorded", c "prediction" "predictionId" None noExtra [ One("hypothesisId", "hypothesis") ]
        "ActionRecorded", c "action" "actionId" (Some "proposed") noExtra [ Many("targetHypothesisIds", "hypothesis") ]
        "FindingRecorded", c "finding" "findingId" None noExtra [ One("actionId", "action") ]
        "NegativeKnowledgeEstablished", c "negativeKnowledge" "negativeKnowledgeId" None noExtra [ Many("basisEvidenceIds", "evidence") ]
        "ContradictionRecorded", c "contradiction" "contradictionId" (Some "detected") (fun _ -> [ "basis", JNull ]) [ Many("evidenceIds", "evidence") ]
        "UnknownIdentified", c "unknown" "unknownId" (Some "identified") (fun _ -> [ "basis", JNull ]) []
        "AssumptionRecorded", c "assumption" "assumptionId" (Some "explicitUnverified") noExtra []
        "CausalAssertionRecorded", c "cause" "causeId" (Some "proposed") noExtra [ MemberHypotheses ]
        "CorrectiveActionRecorded",
        c "correctiveAction" "correctiveActionId" (Some "proposed") (fun _ -> [ "awaitingSinceVersion", JNull ]) [ Many("targetCauseIds", "cause") ]
        "CriterionRecorded", c "criterion" "criterionId" None noExtra [ One("correctiveActionId", "correctiveAction") ]
        "VerificationRecorded",
        c "verification" "verificationId" None (fun e -> [ "recordedAtVersion", jint e.AggregateVersion ]) [ One("criterionId", "criterion"); Many("evidenceIds", "evidence") ]
        "ConclusionRecorded", c "conclusion" "conclusionId" None (fun e -> [ "recordedAtVersion", jint e.AggregateVersion ]) [ Many("causeIds", "cause") ]
    ]

let private pairs (froms: string list) (tos: string list) = [ for f in froms do for t in tos -> f, t ]

/// Legal (from, to) pairs per lifecycle (docs/STATE-TRANSITION-TABLES.md §3-§11).
let transitions : Map<string, Set<string * string>> =
    Map.ofList [
        "evidence", set [ for f in Enums.evidenceVerification do for t in Enums.evidenceVerification do if f <> t then yield f, t ]
        "unknown", set ([ "identified", "investigating" ] @ pairs [ "identified"; "investigating" ] [ "resolved"; "acceptedResidual"; "superseded" ])
        "contradiction",
        set (
            [ "detected", "triaged" ]
            @ pairs [ "detected"; "triaged" ] [ "investigating" ]
            @ pairs [ "detected"; "triaged"; "investigating" ] [ "resolved"; "acceptedResidual"; "superseded" ])
        "assumption",
        set (
            [ "explicitUnverified", "provisionallyAccepted" ]
            @ pairs [ "explicitUnverified"; "provisionallyAccepted" ] [ "verified"; "rejected" ]
            @ pairs [ "explicitUnverified"; "provisionallyAccepted"; "verified" ] [ "expired" ])
        "action",
        set (
            [ "proposed", "approved"; "proposed", "rejected"; "approved", "inProgress"; "inProgress", "completed"; "completed", "interpreted" ]
            @ pairs [ "approved"; "inProgress" ] [ "blocked"; "cancelled" ]
            @ pairs [ "blocked" ] [ "approved"; "inProgress" ])
        "correctiveAction",
        set (
            [ "proposed", "approved"; "approved", "inProgress"; "inProgress", "implemented"; "implemented", "awaitingVerificationOfEffect"
              "awaitingVerificationOfEffect", "verifiedEffective"; "awaitingVerificationOfEffect", "verificationFailed"
              "verificationFailed", "reworkRequired"; "reworkRequired", "inProgress" ]
            @ pairs [ "proposed"; "approved" ] [ "cancelled" ])
    ]

/// `*Transitioned` events that carry only the target state: entity kind, id field, extra fields copied onto the entity.
let stateChanges : Map<string, string * string * string list> =
    Map.ofList [
        "EvidenceVerificationAssessed", ("evidence", "evidenceId", [])
        "ActionTransitioned", ("action", "actionId", [])
        "ContradictionTransitioned", ("contradiction", "contradictionId", [ "basis" ])
        "UnknownTransitioned", ("unknown", "unknownId", [ "basis" ])
        "AssumptionTransitioned", ("assumption", "assumptionId", [])
        "CorrectiveActionTransitioned", ("correctiveAction", "correctiveActionId", [])
    ]

/// Hypothesis commands and the (from, to) pairs each allows (§2).
let hypothesisCommands : (string * (string * string) list) list =
    let open4 = [ "proposed"; "plausible"; "supported"; "weakened" ]
    [ "markPlausible", [ "proposed", "plausible"; "weakened", "plausible" ]
      "markSupported", [ "plausible", "supported"; "weakened", "supported" ]
      "markWeakened", [ "plausible", "weakened"; "supported", "weakened" ]
      "falsify", pairs open4 [ "falsified" ]
      "supersede", pairs open4 [ "superseded" ]
      "reopen", [ "falsified", "plausible" ] ]

/// Investigation lifecycle commands: legal source states and the target state (§1).
let lifecycleCommands : (string * (string list * string)) list =
    let nonArchived = Enums.lifecycle |> List.filter ((<>) "archived")
    [ "activateInvestigation", ([ "draft"; "awaitingEvidence"; "archived" ], "active")
      "markAwaitingEvidence", ([ "active" ], "awaitingEvidence")
      "markAwaitingVerification", ([ "active" ], "awaitingVerification")
      "verificationFailed", ([ "awaitingVerification" ], "active")
      "resolveInvestigation", ([ "active"; "awaitingVerification" ], "resolved")
      "closeInvestigation", ([ "resolved" ], "closed")
      "reopenInvestigation", ([ "resolved"; "closed" ], "active")
      "archiveInvestigation", (nonArchived, "archived") ]

let private text name payload = match field name payload with Some(JString s) -> s | _ -> ""

let private requireEntity (inv: Investigation) (kind: string) (id: string) =
    match Map.tryFind id inv.Entities with
    | Some e when e.Kind = kind -> Ok e
    | Some _ -> Error(sprintf "%s is not a %s" id kind)
    | None -> Error(sprintf "unknown %s %s" kind id)

let private requireAll (inv: Investigation) kind (ids: Json option) =
    match ids with
    | Some(JArray xs) -> xs |> Result.traverse (function JString id -> requireEntity inv kind id | _ -> Error "bad id") |> Result.map ignore
    | _ -> Ok()

let private checkReference inv payload reference =
    match reference with
    | One(name, kind) -> requireEntity inv kind (text name payload) |> Result.map ignore
    | Many(name, kind) -> requireAll inv kind (field name payload)
    | MemberHypotheses ->
        match field "members" payload with
        | Some(JArray ms) -> ms |> Result.traverse (fun m -> requireEntity inv "hypothesis" (text "hypothesisId" m)) |> Result.map ignore
        | _ -> Error "members missing"

let private writable (inv: Investigation) =
    Result.require (inv.Lifecycle <> "archived" && inv.Lifecycle <> "closed") (sprintf "the investigation is %s" inv.Lifecycle)

let provenance (env: Envelope) = JObject [ "actor", env.Actor; "recordedAt", JString env.OccurredAt ]

let private newEntity (env: Envelope) kind id initial (fields: (string * Json) list) =
    { Id = id; Kind = kind; State = initial; Fields = fields @ [ "provenance", provenance env ]; RecordedVersion = env.AggregateVersion }

let private applyCreation inv (env: Envelope) (c: Creation) =
    result {
        do! writable inv
        let id = text c.IdField env.Payload
        do! Result.require (not (Map.containsKey id inv.Entities)) (sprintf "%s %s already exists" c.Kind id)
        do! c.References |> Result.traverse (checkReference inv env.Payload) |> Result.map ignore
        let fields = match env.Payload with JObject fs -> fs |> List.filter (fst >> (<>) c.IdField) | _ -> []
        return { inv with Entities = Map.add id (newEntity env c.Kind id c.Initial (fields @ c.Extra env)) inv.Entities }
    }

let private setField (k: string) (v: Json) (fields: (string * Json) list) =
    if fields |> List.exists (fst >> (=) k) then fields |> List.map (fun (k', v') -> if k' = k then k', v else k', v') else fields @ [ k, v ]

let private applyStateChange inv (env: Envelope) (kind, idField, copied: string list) =
    result {
        do! writable inv
        let! entity = requireEntity inv kind (text idField env.Payload)
        let target = text (stateKey kind) env.Payload
        let current = defaultArg entity.State ""
        do! Result.require (Set.contains (current, target) transitions.[kind]) (sprintf "%s cannot move from %s to %s" kind current target)
        let fields = copied |> List.fold (fun fs k -> setField k (defaultArg (field k env.Payload) JNull) fs) entity.Fields
        let fields = if kind = "correctiveAction" && target = "awaitingVerificationOfEffect" then setField "awaitingSinceVersion" (jint env.AggregateVersion) fields else fields
        return { inv with Entities = Map.add entity.Id { entity with State = Some target; Fields = fields } inv.Entities }
    }

let private applyHypothesis inv (env: Envelope) =
    result {
        do! writable inv
        let! h = requireEntity inv "hypothesis" (text "hypothesisId" env.Payload)
        let command, fromState, toState = text "command" env.Payload, text "from" env.Payload, text "to" env.Payload
        do! Result.require (h.State = Some fromState) (sprintf "hypothesis %s is %s, not %s" h.Id (defaultArg h.State "-") fromState)
        let allowed = hypothesisCommands |> List.tryFind (fst >> (=) command) |> Option.map snd |> Option.defaultValue []
        do! Result.require (List.contains (fromState, toState) allowed) (sprintf "%s cannot move a hypothesis from %s to %s" command fromState toState)
        do! requireAll inv "evidence" (field "evidenceIds" env.Payload)
        let entry =
            JObject [
                "aggregateVersion", jint env.AggregateVersion; "at", JString env.OccurredAt; "command", JString command
                "evidenceIds", defaultArg (field "evidenceIds" env.Payload) (JArray []); "from", JString fromState
                "reason", defaultArg (field "reason" env.Payload) JNull; "to", JString toState ]
        let history = match h.Fields |> List.tryFind (fst >> (=) "history") with Some(_, JArray xs) -> xs | _ -> []
        let updated = { h with State = Some toState; Fields = setField "history" (JArray(history @ [ entry ])) h.Fields }
        return { inv with Entities = Map.add h.Id updated inv.Entities }
    }

let private applyLifecycle inv (env: Envelope) =
    result {
        let command, fromState, toState = text "command" env.Payload, text "from" env.Payload, text "to" env.Payload
        do! Result.require (fromState = inv.Lifecycle) (sprintf "the investigation is %s, not %s" inv.Lifecycle fromState)
        match lifecycleCommands |> List.tryFind (fst >> (=) command) with
        | Some(_, (froms, target)) ->
            do! Result.require (List.contains fromState froms && toState = target) (sprintf "%s cannot move the investigation from %s to %s" command fromState toState)
            return { inv with Lifecycle = toState }
        | None -> return! Error("unknown lifecycle command " + command)
    }

let private applyConfirmation inv (env: Envelope) =
    result {
        do! writable inv
        let! cause = requireEntity inv "cause" (text "causeId" env.Payload)
        do! Result.require (cause.State = Some "proposed" || cause.State = Some "reopened") (sprintf "cause %s is %s and cannot be confirmed" cause.Id (defaultArg cause.State "-"))
        return { inv with Entities = Map.add cause.Id { cause with State = Some "confirmed" } inv.Entities }
    }

let private applyBody (inv: Investigation) (env: Envelope) : Result<Investigation, string> =
    match env.EventType with
    | "InvestigationStarted" -> Error "the investigation has already started"
    | "LifecycleChanged" -> applyLifecycle inv env
    | "HypothesisTransitioned" -> applyHypothesis inv env
    | "CausalAssertionConfirmed" -> applyConfirmation inv env
    | t when creations.ContainsKey t -> applyCreation inv env creations.[t]
    | t when stateChanges.ContainsKey t -> applyStateChange inv env stateChanges.[t]
    | t -> Error("unknown event type " + t)

/// Applies one envelope to the state (None before the first event). Fails closed.
let evolve (state: Investigation option) (env: Envelope) : Result<Investigation, string> =
    result {
        do! validatePayload env.EventType env.Payload
        match state with
        | None ->
            do! Result.require (env.EventType = "InvestigationStarted") "history must begin with InvestigationStarted"
            do! Result.require (env.AggregateVersion = 1L) "the first event must have aggregateVersion 1"
            let observationId = text "observationId" env.Payload
            let observation = newEntity env "observation" observationId None [ "observedAt", JNull; "original", JString(text "original" env.Payload) ]
            return
                { Id = env.InvestigationId
                  Title = text "title" env.Payload
                  OriginalProblem = text "original" env.Payload
                  Lifecycle = "draft"
                  Version = 1L
                  CreatedAt = env.OccurredAt
                  UpdatedAt = env.OccurredAt
                  Entities = Map.ofList [ observationId, observation ]
                  EventIds = Set.singleton env.EventId }
        | Some inv ->
            do! Result.require (env.InvestigationId = inv.Id) "event belongs to another investigation"
            do! Result.require (env.AggregateVersion = inv.Version + 1L) (sprintf "aggregateVersion %d does not follow %d" env.AggregateVersion inv.Version)
            do! Result.require (not (inv.EventIds.Contains env.EventId)) ("duplicate eventId " + env.EventId)
            let! next = applyBody inv env
            return { next with Version = env.AggregateVersion; UpdatedAt = env.OccurredAt; EventIds = next.EventIds.Add env.EventId }
    }
    |> Result.mapError (fun e -> sprintf "event %d (%s): %s" env.AggregateVersion env.EventType e)

let replay (events: Envelope list) : Result<Investigation, string> =
    match events with
    | [] -> Error "the history is empty"
    | first :: rest -> evolve None first |> Result.bind (fun s -> Result.foldM (fun inv e -> evolve (Some inv) e) s rest)

// ---- Commands ----

type Actor = { ActorId: string; DisplayName: string option }

/// A user intention. For HypothesisTransitioned and LifecycleChanged the payload names only the target (`to`);
/// `decide` adds `from` and `command` from the current state, so re-deciding a command on a newer version
/// (conflict reapply) uses that version's state.
type Command = { EventType: string; Payload: Json }

type Context = { NewId: unit -> string; Now: unit -> string; Actor: Actor }

let private replaceFields (extra: (string * Json) list) (payload: Json) =
    match payload with
    | JObject fields ->
        let names = List.map fst extra
        JObject((fields |> List.filter (fun (k, _) -> not (List.contains k names))) @ extra)
    | other -> other

let private complete (state: Investigation option) (cmd: Command) : Result<Json, string> =
    match cmd.EventType, state with
    | "HypothesisTransitioned", Some inv ->
        let id, target = text "hypothesisId" cmd.Payload, text "to" cmd.Payload
        match Map.tryFind id inv.Entities |> Option.bind (fun e -> e.State) with
        | Some current ->
            match hypothesisCommands |> List.tryFind (fun (_, ps) -> List.contains (current, target) ps) with
            | Some(command, _) -> Ok(replaceFields [ "command", JString command; "from", JString current ] cmd.Payload)
            | None -> Error(sprintf "a hypothesis cannot move from %s to %s" current target)
        | None -> Error(sprintf "unknown hypothesis %s" id)
    | "LifecycleChanged", Some inv ->
        let target = text "to" cmd.Payload
        match lifecycleCommands |> List.tryFind (fun (_, (froms, t)) -> t = target && List.contains inv.Lifecycle froms) with
        | Some(command, _) -> Ok(replaceFields [ "command", JString command; "from", JString inv.Lifecycle ] cmd.Payload)
        | None -> Error(sprintf "the investigation cannot move from %s to %s" inv.Lifecycle target)
    | _ -> Ok cmd.Payload

let envelopeJson (env: Envelope) (correlationId: string) =
    JObject [
        "formatVersion", JString "1.0.0"
        "eventId", JString env.EventId
        "investigationId", JString env.InvestigationId
        "aggregateVersion", jint env.AggregateVersion
        "eventType", JString env.EventType
        "occurredAt", JString env.OccurredAt
        "actor", env.Actor
        "semanticVersion", JString "1.0.0"
        "correlationId", JString correlationId
        "payload", env.Payload
    ]

let actorJson (a: Actor) =
    JObject([ "actorId", JString a.ActorId; "kind", JString "human" ] @ (match a.DisplayName with Some d -> [ "displayName", JString d ] | None -> []))

/// Turns a command into the envelope it records, or the engine's refusal reason.
let decide (ctx: Context) (investigationId: string) (state: Investigation option) (cmd: Command) : Result<Envelope * Investigation, string> =
    result {
        let! payload = complete state cmd
        let env0 =
            { EventId = ctx.NewId()
              InvestigationId = investigationId
              AggregateVersion = (state |> Option.map (fun s -> s.Version) |> Option.defaultValue 0L) + 1L
              EventType = cmd.EventType
              OccurredAt = ctx.Now()
              Actor = actorJson ctx.Actor
              Payload = payload
              Raw = JNull }
        let env = { env0 with Raw = envelopeJson env0 (ctx.NewId()) }
        let! next = evolve state env
        return env, next
    }

let private valuesOf kind =
    match kind with
    | "hypothesis" -> Enums.hypothesis | "evidence" -> Enums.evidenceVerification | "unknown" -> Enums.unknown
    | "contradiction" -> Enums.contradiction | "action" -> Enums.action | "assumption" -> Enums.assumption
    | "correctiveAction" -> Enums.corrective | _ -> []

/// Commands that could move an entity now, found by asking `evolve` (never re-derived): (eventType, target).
let legalTargets (ctx: Context) (inv: Investigation) (entity: Entity) : (string * string) list =
    let attempt eventType payload = Result.isOk (decide ctx inv.Id (Some inv) { EventType = eventType; Payload = payload })
    match entity.Kind with
    | "hypothesis" ->
        Enums.hypothesis
        |> List.filter (fun t -> attempt "HypothesisTransitioned" (JObject [ "hypothesisId", JString entity.Id; "to", JString t; "reason", JString "check"; "evidenceIds", JArray [] ]))
        |> List.map (fun t -> "HypothesisTransitioned", t)
    | "cause" ->
        if attempt "CausalAssertionConfirmed" (JObject [ "causeId", JString entity.Id; "policy", JString "core-default/1" ]) then [ "CausalAssertionConfirmed", "confirmed" ] else []
    | kind ->
        match stateChanges |> Map.toList |> List.tryFind (fun (_, (k, _, _)) -> k = kind) with
        | Some(eventType, (_, idField, copied)) ->
            valuesOf kind
            |> List.filter (fun t ->
                attempt eventType (JObject([ idField, JString entity.Id; stateKey kind, JString t ] @ (copied |> List.map (fun c -> c, JString "check")))))
            |> List.map (fun t -> eventType, t)
        | None -> []

/// Lifecycle targets the engine would accept now.
let legalLifecycle (ctx: Context) (inv: Investigation) : string list =
    Enums.lifecycle
    |> List.filter (fun t -> Result.isOk (decide ctx inv.Id (Some inv) { EventType = "LifecycleChanged"; Payload = JObject [ "to", JString t; "reason", JString "check" ] }))
