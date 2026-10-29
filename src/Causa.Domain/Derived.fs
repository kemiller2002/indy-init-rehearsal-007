/// Derived, presentation-only facts computed from the replayed state (never persisted, never authority).
module Causa.Domain.Derived

open Causa.Domain.Json
open Causa.Domain.Engine

type NextStep = { EntityId: string; Kind: string; Text: string }

let private ofKind kind (inv: Investigation) = inv.Entities |> Map.toList |> List.map snd |> List.filter (fun e -> e.Kind = kind)

let private label (e: Entity) =
    [ "statement"; "question"; "outcome"; "description" ]
    |> List.tryPick (fun k -> e.Fields |> List.tryFind (fst >> (=) k) |> Option.bind (snd >> function JString s -> Some s | _ -> None))
    |> Option.defaultValue e.Id

let private fieldText k (e: Entity) = e.Fields |> List.tryFind (fst >> (=) k) |> Option.bind (snd >> function JString s -> Some s | _ -> None)

let unresolvedContradictions (inv: Investigation) =
    ofKind "contradiction" inv |> List.filter (fun e -> not (List.contains (defaultArg e.State "") [ "resolved"; "acceptedResidual"; "superseded" ]))

/// UI-032: next steps that follow from the records themselves; each names the record it comes from.
let nextSteps (inv: Investigation) : NextStep list =
    let relatedHypotheses = ofKind "relationship" inv |> List.choose (fieldText "hypothesisId") |> Set.ofList
    let openStates = [ "proposed"; "plausible"; "supported"; "weakened" ]
    [ for e in unresolvedContradictions inv -> { EntityId = e.Id; Kind = "contradiction"; Text = "Resolve or accept the contradiction: " + label e }
      for e in ofKind "unknown" inv do
          if List.contains (defaultArg e.State "") [ "identified"; "investigating" ] && List.contains (defaultArg (fieldText "materiality" e) "") [ "blocking"; "material" ] then
              yield { EntityId = e.Id; Kind = "unknown"; Text = "Answer the open question: " + label e }
      for e in ofKind "hypothesis" inv do
          if List.contains (defaultArg e.State "") openStates && not (relatedHypotheses.Contains e.Id) then
              yield { EntityId = e.Id; Kind = "hypothesis"; Text = "Relate evidence to the hypothesis: " + label e }
      for e in ofKind "correctiveAction" inv do
          if e.State = Some "awaitingVerification" then yield { EntityId = e.Id; Kind = "correctiveAction"; Text = "Verify the corrective action: " + label e }
      let confirmed = ofKind "cause" inv |> List.filter (fun e -> e.State = Some "confirmed")
      if not confirmed.IsEmpty && (ofKind "conclusion" inv).IsEmpty then
          yield { EntityId = confirmed.Head.Id; Kind = "cause"; Text = "Record a conclusion for the confirmed cause: " + label confirmed.Head } ]

let count kind (inv: Investigation) = (ofKind kind inv).Length
