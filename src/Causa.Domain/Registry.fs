/// The Causa v1.0.0 event payload registry (causa-data contract §7) as data:
/// each event type lists its exact payload fields. Unknown types and fields fail closed.
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
    let hypothesis = [ "proposed"; "plausible"; "supported"; "weakened"; "falsified"; "superseded" ]
    let evidenceCategory = [ "directObservation"; "measurement"; "log"; "document"; "experimentResult"; "testimony"; "externalReference" ]
    let evidenceVerification = [ "unverified"; "partiallyVerified"; "verified"; "invalidated" ]
    let relationship = [ "supports"; "contradicts"; "discriminates"; "insufficient" ]
    let materiality = [ "blocking"; "material"; "nonMaterial"; "informational" ]
    let verificationOutcome = [ "satisfied"; "failed"; "inconclusive" ]
    let unknown = [ "identified"; "investigating"; "resolved"; "acceptedResidual"; "superseded" ]
    let contradiction = [ "detected"; "triaged"; "investigating"; "resolved"; "acceptedResidual"; "superseded" ]
    let action = [ "proposed"; "approved"; "inProgress"; "blocked"; "completed"; "interpreted"; "rejected"; "cancelled" ]
    let cause = [ "proposed"; "confirmed"; "reopened"; "superseded" ]
    let corrective = [ "proposed"; "approved"; "inProgress"; "implemented"; "awaitingVerification"; "verifiedEffective"; "verificationFailed"; "reworkRequired"; "cancelled" ]
    let contentKind = [ "text"; "artifact" ]
    let lifecycle = [ "draft"; "active"; "awaitingEvidence"; "awaitingVerification"; "resolved"; "closed"; "archived" ]

let private reason = "reason", Text

let registry : Map<string, (string * FieldKind) list> =
    Map.ofList [
        "InvestigationStarted", [ "title", Text; "originalProblem", Text ]
        "InvestigationRenamed", [ "title", Text ]
        "InvestigationActivated", [ reason ]
        "InvestigationMarkedAwaitingEvidence", [ reason ]
        "InvestigationMarkedAwaitingVerification", [ reason ]
        "InvestigationResolved", [ reason; "conclusionId", Uuid ]
        "InvestigationClosed", [ reason ]
        "InvestigationReopened", [ reason ]
        "InvestigationArchived", [ reason ]
        "ObservationRecorded", [ "observationId", Uuid; "original", Text; "observedAt", TimestampOrNull ]
        "HypothesisProposed", [ "hypothesisId", Uuid; "statement", Text; "mechanism", TextOrNull ]
        "HypothesisStateChanged", [ "hypothesisId", Uuid; "from", Enum Enums.hypothesis; "to", Enum Enums.hypothesis; reason; "evidenceIds", UuidList ]
        "EvidenceAdmitted",
        [ "evidenceId", Uuid; "contentKind", Enum Enums.contentKind; "content", Text; "category", Enum Enums.evidenceCategory
          "source", Text; "capturedAt", TimestampOrNull; "verification", Enum Enums.evidenceVerification ]
        "EvidenceVerificationChanged", [ "evidenceId", Uuid; "from", Enum Enums.evidenceVerification; "to", Enum Enums.evidenceVerification; reason ]
        "EvidenceRelatedToHypothesis", [ "relationshipId", Uuid; "evidenceId", Uuid; "hypothesisId", Uuid; "kind", Enum Enums.relationship; "rationale", TextOrNull ]
        "UnknownIdentified", [ "unknownId", Uuid; "question", Text; "materiality", Enum Enums.materiality ]
        "UnknownStateChanged", [ "unknownId", Uuid; "from", Enum Enums.unknown; "to", Enum Enums.unknown; "basis", Text ]
        "AssumptionRecorded", [ "assumptionId", Uuid; "proposition", Text; "materiality", Enum Enums.materiality ]
        "ContradictionDetected", [ "contradictionId", Uuid; "statement", Text; "evidenceIds", UuidList; "materiality", Enum Enums.materiality ]
        "ContradictionStateChanged", [ "contradictionId", Uuid; "from", Enum Enums.contradiction; "to", Enum Enums.contradiction; "basis", Text ]
        "PredictionRecorded", [ "predictionId", Uuid; "hypothesisId", Uuid; "statement", Text; "strength", Text; "timing", Text ]
        "InvestigationActionProposed", [ "actionId", Uuid; "kind", Text; "purpose", Text; "targetHypothesisIds", UuidList ]
        "InvestigationActionStateChanged", [ "actionId", Uuid; "from", Enum Enums.action; "to", Enum Enums.action; reason ]
        "FindingRecorded", [ "findingId", Uuid; "actionId", Uuid; "originalResult", Text ]
        "CausalAssertionProposed", [ "causeId", Uuid; "outcome", Text; "members", Members; "mechanism", TextOrNull ]
        "CausalAssertionStateChanged", [ "causeId", Uuid; "from", Enum Enums.cause; "to", Enum Enums.cause; reason ]
        "CorrectiveActionProposed", [ "correctiveActionId", Uuid; "kind", Text; "description", Text; "targetCauseIds", UuidList ]
        "CorrectiveActionStateChanged", [ "correctiveActionId", Uuid; "from", Enum Enums.corrective; "to", Enum Enums.corrective; reason ]
        "VerificationCriterionRecorded", [ "criterionId", Uuid; "correctiveActionId", Uuid; "expectedOutcome", Text; "method", TextOrNull ]
        "VerificationRecorded", [ "verificationId", Uuid; "criterionId", Uuid; "outcome", Enum Enums.verificationOutcome; "evidenceIds", UuidList ]
        "ConclusionRecorded", [ "conclusionId", Uuid; "statement", Text; "strength", Text; "causeIds", UuidList; "residualUncertainty", TextOrNull ]
    ]

let private checkUuidList name =
    function
    | Some(JArray items) ->
        let ids = items |> List.choose (function JString s when Text.isUuid s -> Some s | _ -> None)
        if ids.Length <> items.Length then Error(name + " must hold lowercase UUIDs")
        elif ids <> (ids |> List.distinct |> List.sort) then Error(name + " must be sorted and unique")
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
                let! _ = nonEmpty "role" m
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
