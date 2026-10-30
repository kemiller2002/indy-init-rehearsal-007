/// The Causa v1.0.0 event payload registry as data: each event type lists its exact payload fields.
/// Field names, event types and enumeration values follow the frozen causa-storage v1.0.0 contract as
/// its released data uses them (KD-017). Unknown types and fields fail closed.
module Causa.Domain.Registry

open Causa.Domain.Json

type FieldKind =
    | Text
    | TextOrNull
    | Uuid
    | UuidList
    | TimestampOrNull
    | Enum of string list
    | Members

module Enums =
    let lifecycle = [ "draft"; "active"; "awaitingEvidence"; "awaitingVerification"; "resolved"; "closed"; "archived" ]
    let lifecycleCommands =
        [ "activateInvestigation"; "markAwaitingEvidence"; "markAwaitingVerification"; "resolveInvestigation"; "verificationFailed"
          "closeInvestigation"; "reopenInvestigation"; "archiveInvestigation" ]
    let hypothesis = [ "proposed"; "plausible"; "supported"; "weakened"; "falsified"; "superseded" ]
    let hypothesisCommands = [ "markPlausible"; "markSupported"; "markWeakened"; "falsify"; "supersede"; "reopen" ]
    let evidenceCategory = [ "directObservation"; "measurement"; "log"; "document"; "experimentResult"; "testimony"; "externalReference" ]
    let evidenceVerification = [ "unverified"; "partiallyVerified"; "verified"; "invalidated" ]
    let relationship = [ "supports"; "contradicts"; "discriminates"; "insufficient" ]
    let materiality = [ "blocking"; "material"; "nonMaterial"; "informational" ]
    let verificationOutcome = [ "satisfied"; "failed"; "inconclusive" ]
    let unknown = [ "identified"; "investigating"; "resolved"; "acceptedResidual"; "superseded" ]
    let contradiction = [ "detected"; "triaged"; "investigating"; "resolved"; "acceptedResidual"; "superseded" ]
    let assumption = [ "explicitUnverified"; "provisionallyAccepted"; "verified"; "rejected"; "expired" ]
    let action = [ "proposed"; "approved"; "inProgress"; "blocked"; "completed"; "interpreted"; "rejected"; "cancelled" ]
    let cause = [ "proposed"; "confirmed"; "reopened"; "superseded" ]
    let corrective =
        [ "proposed"; "approved"; "inProgress"; "implemented"; "awaitingVerificationOfEffect"; "verifiedEffective"; "verificationFailed"
          "reworkRequired"; "cancelled" ]
    let contentKind = [ "text"; "artifact" ]
    let causalRole = [ "rootCause"; "trigger"; "contributingFactor"; "enablingCondition" ]
    let actorKind = [ "human"; "agent"; "system"; "external"; "imported" ]

let registry : Map<string, (string * FieldKind) list> =
    Map.ofList [
        "InvestigationStarted", [ "title", Text; "observationId", Uuid; "original", Text ]
        "LifecycleChanged", [ "command", Enum Enums.lifecycleCommands; "from", Enum Enums.lifecycle; "to", Enum Enums.lifecycle; "reason", TextOrNull ]
        "ObservationRecorded", [ "observationId", Uuid; "original", Text; "observedAt", TimestampOrNull ]
        "HypothesisRecorded", [ "hypothesisId", Uuid; "statement", Text; "mechanism", TextOrNull ]
        "HypothesisTransitioned",
        [ "hypothesisId", Uuid; "command", Enum Enums.hypothesisCommands; "from", Enum Enums.hypothesis; "to", Enum Enums.hypothesis
          "reason", Text; "evidenceIds", UuidList ]
        "EvidenceAdmitted",
        [ "evidenceId", Uuid; "contentKind", Enum Enums.contentKind; "content", Text; "category", Enum Enums.evidenceCategory
          "source", Text; "capturedAt", TimestampOrNull ]
        "EvidenceVerificationAssessed", [ "evidenceId", Uuid; "verification", Enum Enums.evidenceVerification ]
        "EvidenceRelated", [ "relationshipId", Uuid; "evidenceId", Uuid; "hypothesisId", Uuid; "kind", Enum Enums.relationship; "rationale", TextOrNull ]
        "PredictionRecorded", [ "predictionId", Uuid; "hypothesisId", Uuid; "statement", Text; "strength", Text; "timing", Text ]
        "ActionRecorded", [ "actionId", Uuid; "kind", Text; "purpose", Text; "targetHypothesisIds", UuidList ]
        "ActionTransitioned", [ "actionId", Uuid; "state", Enum Enums.action ]
        "FindingRecorded", [ "findingId", Uuid; "actionId", Uuid; "originalResult", Text; "interpretation", TextOrNull ]
        "NegativeKnowledgeEstablished", [ "negativeKnowledgeId", Uuid; "statement", Text; "scope", Text; "basisEvidenceIds", UuidList ]
        "ContradictionRecorded", [ "contradictionId", Uuid; "statement", Text; "evidenceIds", UuidList; "materiality", Enum Enums.materiality ]
        "ContradictionTransitioned", [ "contradictionId", Uuid; "state", Enum Enums.contradiction; "basis", Text ]
        "UnknownIdentified", [ "unknownId", Uuid; "question", Text; "materiality", Enum Enums.materiality ]
        "UnknownTransitioned", [ "unknownId", Uuid; "state", Enum Enums.unknown; "basis", Text ]
        "AssumptionRecorded", [ "assumptionId", Uuid; "proposition", Text; "materiality", Enum Enums.materiality ]
        "AssumptionTransitioned", [ "assumptionId", Uuid; "state", Enum Enums.assumption ]
        "CausalAssertionRecorded", [ "causeId", Uuid; "outcome", Text; "members", Members; "mechanism", TextOrNull ]
        "CausalAssertionConfirmed", [ "causeId", Uuid; "policy", Text ]
        "CorrectiveActionRecorded", [ "correctiveActionId", Uuid; "kind", Text; "description", Text; "targetCauseIds", UuidList ]
        "CriterionRecorded", [ "criterionId", Uuid; "correctiveActionId", Uuid; "expectedOutcome", Text; "method", TextOrNull ]
        "CorrectiveActionTransitioned", [ "correctiveActionId", Uuid; "state", Enum Enums.corrective ]
        "VerificationRecorded", [ "verificationId", Uuid; "criterionId", Uuid; "outcome", Enum Enums.verificationOutcome; "evidenceIds", UuidList ]
        "ConclusionRecorded", [ "conclusionId", Uuid; "statement", Text; "strength", Text; "causeIds", UuidList; "residualUncertainty", TextOrNull ]
    ]

let private checkUuidList name =
    function
    | Some(JArray items) ->
        let ids = items |> List.choose (function JString s when Text.isUuid s -> Some s | _ -> None)
        if ids.Length <> items.Length then Error(name + " must hold lowercase UUIDs")
        elif ids.Length <> (List.distinct ids).Length then Error(name + " must not repeat an id")
        else Ok()
    | _ -> Error(name + " must be an array of UUIDs")

let private checkMembers =
    function
    | Some(JArray(_ :: _ as items)) ->
        items
        |> Result.traverse (fun m ->
            result {
                do! onlyKeys [ "hypothesisId"; "role" ] m
                let! h = str "hypothesisId" m
                do! Result.require (Text.isUuid h) "members[].hypothesisId must be a UUID"
                let! role = str "role" m
                do! Result.require (List.contains role Enums.causalRole) "members[].role is not a causal role"
                return ()
            })
        |> Result.map ignore
    | _ -> Error "members must be a nonempty array"

let private checkField (payload: Json) (name: string, kind: FieldKind) : Result<unit, string> =
    let value = field name payload
    match kind, value with
    | _, None -> Error(name + " is required")
    | Text, Some(JString s) when s.Trim() <> "" -> Ok()
    | Text, _ -> Error(name + " must be a nonempty string")
    | TextOrNull, Some JNull -> Ok()
    | TextOrNull, Some(JString s) when s.Trim() <> "" -> Ok()
    | TextOrNull, _ -> Error(name + " must be a nonempty string or null")
    | Uuid, Some(JString s) when Text.isUuid s -> Ok()
    | Uuid, _ -> Error(name + " must be a lowercase UUID")
    | UuidList, v -> checkUuidList name v
    | TimestampOrNull, Some JNull -> Ok()
    | TimestampOrNull, Some(JString s) when Text.isTimestamp s -> Ok()
    | TimestampOrNull, _ -> Error(name + " must be a UTC timestamp or null")
    | Enum values, Some(JString s) when List.contains s values -> Ok()
    | Enum _, _ -> Error(name + " has a value outside its enumeration")
    | Members, v -> checkMembers v

/// Validates a payload against its event type's exact field list.
let validatePayload (eventType: string) (payload: Json) : Result<unit, string> =
    match Map.tryFind eventType registry, payload with
    | None, _ -> Error("unknown event type " + eventType)
    | Some _, p when (match p with JObject _ -> false | _ -> true) -> Error "payload must be an object"
    | Some fields, p ->
        result {
            do! onlyKeys (List.map fst fields) p
            do! fields |> Result.traverse (checkField p) |> Result.map ignore
        }
        |> Result.mapError (fun e -> eventType + ": " + e)
