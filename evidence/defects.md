# Defect and remediation summary (Rehearsal 7)

Every defect found during the timed run, with how it was found, what fixed it and what now guards it.
"Mutation check" means the fix was reverted in place and the named test was observed to fail.

## Found by this agent during the build

| ID | Defect | Found by | Fix | Guard |
|---|---|---|---|---|
| D-01 | CI steps piped through `tee` could not fail (no `pipefail`) | catalog step green although `tools/catalog.fsx` did not compile (run 40978226573) | workflow default shell `bash` (KD-016) | every later run; earlier green results re-checked from their own output |
| D-02 | Registry and snapshot projection followed the candidate contract, not the frozen causa-storage v1.0.0 data | live catalog check refused 4/4 cases | registry, envelope and projection aligned (KD-017, KD-019) | live catalog replay 4/4 to the catalog's state digests |
| D-03 | A commit applied with its answer lost could end in Conflict when reconciliation lagged | live PAT-06 (run 40983572724) | save remembers its markers; own-write check before Conflict (KD-018) | "PAT-06 applied but not yet visible to reconciliation", mutation-checked |
| D-04 | GitHub secondary rate limits reported as a permission refusal; live writers overlapped | main run 40987940706 | RateLimit classification; deployed-acceptance waits for live-provider (KD-020) | unit test in CopyTests |
| D-05 | Evidence template had two root elements, hiding the relate form | browser tests | one root per template | browser suite |
| D-06 | A MutationObserver loop froze the Administration page | browser tests | observer removed | browser suite |
| D-07 | Loading or refused sessions showed Save and raised the leave guard | browser tests | gated by `isLoaded` | PAT-09 |

## Found by the independent review (verdict FAIL, 2026-10-30T02:16Z)

The reviewer was a fresh read-only agent. Its verdict is recorded as given; the fixes below do not turn it into a PASS.
A new independent verification is required before any acceptance claim (see the final report).

| Finding | Severity | Status | Fix | Regression test (Expecto `causa.review remediation`, `causa.domain`) | Mutation check |
|---|---|---|---|---|---|
| F1 | Critical | Fixed | `Confirmation.ofReadBack` and `ofVerifiedRead` are `internal` to Causa.Persistence; loading goes through the new `Load` module, which verifies the bytes GitHub answered before confirming them; conflict reapply/discard use the confirmation from that load | `F1: a load confirms only bytes that pass full verification`; PAT-12 negative now has three projects (record construction, `ofReadBack`, `ofVerifiedRead`), each must fail with its own error (FS1093/FS1094) | making `ofReadBack` public again turns PAT-12 negative to FAIL |
| F2 | Critical | Fixed | `holdsWork` guards refresh at request and again when the load completes: unsaved edits, a save in flight or an unresolved conflict are never replaced | `F2: an edit made while a refresh is in flight is kept…`; `F2: refresh is refused while there are unsaved changes` | killed (2 tests) |
| F3 (M1) | High | Fixed (tests) | the read-back already existed; it was untested | `M1: a commit is confirmed only by reading its bytes back at the landed commit`; `M1: a read-back that differs … is Save failed` | M1 (compare with itself) killed; M1b (no read-back) killed |
| F4 | High | Partly addressed | every-request-lost now proven to end in Unknown commit outcome awaiting the user with one mutation, never a conflict; the reviewer's RepositoryKeptChanging path (lost answers combined with a moving head over 4 mutations) is the PERSIST bound on mutations and is kept | `M2: when every request is lost…` | M2, M2b killed |
| F5 | High | Fixed | disconnect keeps the tab's write-marker sequence | `F5: write markers stay unique across a disconnect and reconnect` | killed |
| F6 | High | Fixed | the app reader now requires the snapshot `state` to equal the replayed state exactly (`diffPaths`), and the manifest and snapshot to carry exactly their required fields | `refuses a resealed snapshot whose entity state disagrees…`, `…with an invented state field`, `refuses a manifest missing a required field`, plus a positive reseal control | F6 and F6b killed; the live catalog check re-runs this reader against causa-data in CI |
| F7 (M2) | High | Fixed (tests) | — | `M2: … never a conflict …`; `M2: … refused before sending, the save is Offline` | killed |
| F8 (M7) | Medium | Fixed (tests) | — | `M7: automatic reconciliation reads are spaced 2, 4 and 8 seconds apart` | killed |
| F9 | Medium | Fixed | the own-write check refuses a comparison that is not `ahead`/`identical` as rewritten history | `F9: an own-write check that finds the base diverged is rewritten history` | killed |
| F10 | Medium | Open | compare results are not paginated (GitHub returns at most 250 commits); a save whose base is more than 250 commits behind could miss its own marker and report Conflict, which preserves the user's work | — | — |
| F11 | Medium | Open | files over 1 MB cannot be read through the contents API; a landed save then stays Unknown commit outcome (never falsely Saved) | — | — |
| F12 | Low | Open | "Last saved" shows when the confirmation was made (load or read-back time), not the commit's time | — | — |
| F13 | Medium | Fixed | a queued save is not started while the tab knows it is offline; it waits as Offline | `F13: a save queued behind another is not sent once the tab knows it is offline` | killed |
| F14 | Medium | Fixed | the credential goes only to `https://api.github.com` or the site's own origin (`/github`, the test server's stand-in); CSP `connect-src` no longer lists plaintext localhost | `F14: the credential is never sent to a plaintext or other host` | killed |
| F15 | Low | Fixed | a refresh whose read fails or is refused keeps the loaded investigation (Ready) and reports the fault | `F15: a refresh whose file read fails leaves the investigation loaded` | killed |

Checked by the reviewer and found sound: expected head on every mutation, the investigation path prefix, bounded retries, the memory-only token, no XSS sink.

## Found by the second independent review (verdict FAIL, 2026-10-30T02:55Z, main at 5b70660)

A second fresh read-only reviewer re-reviewed main after the first remediation. Its verdict is recorded as given.
It confirmed the first review's fixes as sound (read-back, state machine, network-never-conflict, bounds, refresh, credential handling, its own five mutants all caught) and found:

| Finding | Severity | Status | Fix | Regression test | Mutation check |
|---|---|---|---|---|---|
| H1 | High | Fixed | a conflict's GitHub version is read again from the branch's current head: automatically when the connection returns, and on "Read the GitHub version again"; the unsaved change is kept throughout | `H1: … read again when the connection returns` (then reapply and save); `H1: Read the GitHub version again reads the branch's current head` | no re-read on reconnect: killed; re-read at the old head: killed (after strengthening the test to move the head) |
| H2 | High | Partly addressed (structural limit) | every Confirmation now names its investigation and the app refuses one for another investigation; opening goes through the save-state machine (`Opened`) instead of setting Saved directly. Remaining: SaveProcess/Load and the `Reply` type are public, so code that fabricates GitHub answers in-process can still drive the protocol to a confirmation. No production path fabricates replies; closing this needs the transport boundary moved into the persistence assembly, not done in this run | `H2: every confirmation is bound to the investigation it confirms`; PAT-12 machine test now admits only confirmation-carrying inputs | — |
| M1 | Medium | Fixed | contract-fixed values are checked: manifest `schemaVersion`, `semanticVersion`, `snapshotVersion`, `domainPacks`; snapshot `formatVersion`, `semanticVersion`, `projectionVersion` | `M1: values the contract fixes are refused when different` (4 forgeries) | killed; the live catalog check re-runs the reader on causa-data |
| L1 | Low | Fixed | an "ahead" compare with no commits waits for another reconciliation read instead of throwing | `L1: …does not crash reconciliation` | killed |
| L2 | Low | Fixed | a save waiting behind another stays in its own state (nothing is sent for it) with a notice, and is moved to Saving only when it starts | `L2: a save waiting behind another does not claim to be sending` | killed |
