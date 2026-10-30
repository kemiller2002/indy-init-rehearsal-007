module Negative.ForgeReadBack

open Causa.Persistence

// Application code outside the persistence module "confirms" its own bytes by comparing them with themselves.
let files : Causa.Domain.FileSet.Files = { Manifest = ""; Events = ""; Snapshot = "" }
let forged = Confirmation.ofReadBack "inv" "0000000000000000000000000000000000000000" files files ""
