module Negative.ConstructSaved

open Causa.Persistence

// Application code outside the persistence module tries to build Saved without a read-back.
let forged : SaveState =
    Saved { InvestigationId = "x"; Commit = "0000000000000000000000000000000000000000"; Files = { Manifest = ""; Events = ""; Snapshot = "" }; ConfirmedAt = "" }
