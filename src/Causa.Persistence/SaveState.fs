namespace Causa.Persistence

open Causa.Domain.FileSet

/// Proof that GitHub holds exactly these bytes at this commit: produced only inside the persistence
/// module, from GitHub's answers to reads at that commit (PERSIST-007, PERSIST-023).
/// The representation is private and both constructors are internal, so application code can obtain
/// one only from a save's read-back (SaveProcess) or a verified load (Load).
type Confirmation = private { Commit: string; Files: Files; ConfirmedAt: string }

module Confirmation =
    let commit (c: Confirmation) = c.Commit
    let files (c: Confirmation) = c.Files
    let confirmedAt (c: Confirmation) = c.ConfirmedAt

    /// A read-back: the bytes read at `commit` must equal `expected` exactly.
    let internal ofReadBack (commit: string) (expected: Files) (read: Files) (at: string) : Result<Confirmation, string> =
        if expected = read then Ok { Commit = commit; Files = read; ConfirmedAt = at }
        else Error "the bytes read back from GitHub differ from the bytes the save wrote"

    /// Opening an investigation: bytes read at `commit` that pass full verification.
    let internal ofVerifiedRead (commit: string) (expectedId: string) (read: Files) (at: string) : Result<Verified * Confirmation, string> =
        verify expectedId read |> Result.map (fun v -> v, { Commit = commit; Files = read; ConfirmedAt = at })

type FailureCause =
    | RepositoryKeptChanging
    | CredentialRefused
    | PermissionDenied
    | RateLimited
    | Rejected of string
    | ReadBackMismatch
    | HistoryRewritten

/// The seven save states (PERSIST-031). Saved carries the confirmation it was built from.
type SaveState =
    | Saved of Confirmation
    | Unsaved
    | Saving
    | Offline
    | Conflict
    | UnknownOutcome
    | SaveFailed of FailureCause

type SaveInput =
    | Edited of online: bool
    | SaveRequested of online: bool
    | NotSent
    | MutationSent
    | StaleUnrelated
    | StaleConflict
    | Confirmed of Confirmation * unsavedRemain: bool * online: bool
    | OutcomeLost
    | Failed of FailureCause
    | NothingWritten
    | ConnectivityLost
    | ConnectivityRestored
    | Discarded of Confirmation
    | Reapplied of Confirmation * anyAccepted: bool * online: bool

type SaveFault = { From: string; Input: string; Reason: string }

module SaveState =
    let name =
        function
        | Saved _ -> "Saved"
        | Unsaved -> "Unsaved"
        | Saving -> "Saving"
        | Offline -> "Offline"
        | Conflict -> "Conflict"
        | UnknownOutcome -> "Unknown commit outcome"
        | SaveFailed _ -> "Save failed"

    let inputName =
        function
        | Edited _ -> "Edited" | SaveRequested _ -> "SaveRequested" | NotSent -> "NotSent" | MutationSent -> "MutationSent"
        | StaleUnrelated -> "StaleUnrelated" | StaleConflict -> "StaleConflict" | Confirmed _ -> "Confirmed"
        | OutcomeLost -> "OutcomeLost" | Failed _ -> "Failed" | NothingWritten -> "NothingWritten"
        | ConnectivityLost -> "ConnectivityLost" | ConnectivityRestored -> "ConnectivityRestored"
        | Discarded _ -> "Discarded" | Reapplied _ -> "Reapplied"

    let private pending online = if online then Unsaved else Offline

    let private afterConfirm c unsavedRemain online = if unsavedRemain then pending online else Saved c

    /// The save-state machine. Every input that is not legal in a state fails closed with a fault
    /// and the caller keeps the old state.
    let step (state: SaveState) (input: SaveInput) : Result<SaveState, SaveFault> =
        let next =
            match state, input with
            | (Saved _ | Unsaved | Offline | SaveFailed _), Edited online -> Some(match state with SaveFailed _ -> state | _ -> pending online)
            | (Saving | UnknownOutcome), Edited _ -> Some state
            | Saved _, (ConnectivityLost | ConnectivityRestored) -> Some state
            | (Unsaved | SaveFailed _), ConnectivityLost -> Some Offline
            | Offline, ConnectivityLost -> Some Offline
            | Offline, ConnectivityRestored -> Some Unsaved
            | Unsaved, ConnectivityRestored -> Some Unsaved
            | SaveFailed _, ConnectivityRestored -> Some state
            | (Unsaved | Offline | SaveFailed _), SaveRequested online -> Some(if online then Saving else Offline)
            | Saving, NotSent -> Some Offline
            | (Saving | UnknownOutcome), (ConnectivityLost | ConnectivityRestored) -> Some state
            | Saving, (MutationSent | StaleUnrelated) -> Some Saving
            | (Saving | UnknownOutcome), StaleConflict -> Some Conflict
            | (Saving | UnknownOutcome), Confirmed(c, remain, online) -> Some(afterConfirm c remain online)
            | (Saving | UnknownOutcome), OutcomeLost -> Some UnknownOutcome
            | (Saving | UnknownOutcome), Failed cause -> Some(SaveFailed cause)
            | UnknownOutcome, (NothingWritten | StaleUnrelated) -> Some Saving
            | Conflict, Discarded c -> Some(Saved c)
            | Conflict, Reapplied(c, anyAccepted, online) -> Some(if anyAccepted then pending online else Saved c)
            | Conflict, (ConnectivityLost | ConnectivityRestored) -> Some Conflict
            | _ -> None
        match next with
        | Some s -> Ok s
        | None -> Error { From = name state; Input = inputName input; Reason = "this input is not legal in this save state" }

    /// Whether the tab must warn before it is closed (PERSIST-019).
    let needsLeaveWarning = function Saved _ -> false | _ -> true
