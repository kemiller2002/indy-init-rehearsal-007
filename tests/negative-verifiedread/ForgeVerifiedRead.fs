module Negative.ForgeVerifiedRead

open Causa.Persistence

// Application code outside the persistence module claims bytes it never read from GitHub were read at a commit.
let files : Causa.Domain.FileSet.Files = { Manifest = ""; Events = ""; Snapshot = "" }
let forged = Confirmation.ofVerifiedRead "0000000000000000000000000000000000000000" "id" files ""
