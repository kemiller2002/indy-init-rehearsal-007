# Rehearsal 7 final report: Causa

| | |
|---|---|
| Timed run | T0 2026-10-29T22:49:55Z, deadline 2026-10-30T05:49:55Z (420 min) |
| Report written | 2026-10-30T05:53Z, **after the deadline**, at the owner's request |
| Baseline | BL-CAUSA-INDY-INIT-8445ca11986f (Indy-init 5fcbffa), read-only throughout |
| Application | kemiller2002/indy-init-rehearsal-007, `main` at **1298489**, deployed to GitHub Pages (https://kevinmmiller.us/indy-init-rehearsal-007/) at that exact SHA |
| Stack | F# (domain, persistence, app engine) · Limen 0.9.1 (WASM runtime) · Forma 0.12.0 (UI) · .NET 10.0.402 |

## 1. Verdict

### Acceptance: NOT VERIFIED, NOT ACCEPTED

- This agent does not certify its own work. Three fresh, read-only reviewer agents each assessed the build independently. Each was given the code and the frozen requirements, not this agent's conclusions.
- Every review returned **FAIL**. No independent review of the final head has passed.
- Acceptance therefore needs a verifier who is not this agent, ideally the human owner. That verifier would re-run acceptance on the final SHA and run the PAT-13 provenance comparison, which this agent is not permitted to run.

| Review | Build reviewed | Verdict | Most serious findings | Disposition |
|---|---|---|---|---|
| 1 | main a8e2040 + #24 | FAIL (15 findings, 3 surviving mutants) | Saved for bytes GitHub never confirmed; a refresh could discard unsaved edits | 11 fixed, 1 partly addressed, 3 open (#25) |
| 2 | main 5b70660 | FAIL (2 high) | a conflict that could not be resolved without losing work; a public route to a confirmation | H1, M1, L1, L2 fixed; H2 partly addressed (#26) |
| 3 | main 1298489 | FAIL (1 high) | a race introduced by the second fix could overwrite another writer's change | H1, M1 fixed in #28, **not merged** at the deadline |

### Deadline position

- **On `main` (1298489):** the build, the live tests and the deploy pass. One test failed in the deployed-site run (see §3).
- **Not merged by the deadline:** the third review's fixes (PR #28). That PR was fully green at 03:51Z but was never merged (§6).
- **Delivery: partial.** Not every deliverable was complete and merged at T+420.

## 2. What was built

- **What Causa does:** it is a root-cause investigation workspace. Investigations are stored as event histories in a GitHub repository the user connects.
- **How each tab works:** each browser tab works on its own in-memory copy and saves with one expected-head commit. The save counts only when the bytes read back from GitHub equal the bytes written. Each tab also handles rebases, conflicts, lost answers and offline periods.

| Layer | Where | Responsibility |
|---|---|---|
| Domain (F#) | `src/Causa.Domain` | canonical JSON (RFC 8785), the frozen v1.0.0 event registry, `decide`/`evolve`/replay, file-set verification (snapshot state must equal replay; contract-fixed values checked) |
| Persistence (F#) | `src/Causa.Persistence` | seven-state save machine, typed GitHub answer classification, the save process (bounded rebases, marker reconciliation spaced 2/4/8 s), verified loads; confirmations constructible only inside this assembly |
| App (F#) | `src/Causa.App` | Limen engine: model, update, view projection |
| Host (C#, 10 lines) | `src/Causa.Host` | the single `[JSExport]` entry point (KD-007) |
| Page | `web/` | Limen `data-*` bindings, Forma CSS, a small browser-boundary script (KD-010) |

The code follows the owner's preference for functional style:
- immutable records and discriminated unions;
- pure `update`, `step` and `onReply` functions;
- effects returned as data.

## 3. Results on the final `main` (1298489, run 40996455117)

| Check | Result |
|---|---|
| Domain, save-state, copy, persistence (stateful fake), F# | PASS (241 at 1298489; 244 on #28) |
| PAT-12 compile-time negative (3 forgeries must not compile) | PASS |
| PAT-13 production artifact scan | PASS |
| Browser acceptance, fake provider, Chromium + WebKit | PASS |
| Live catalog: 4 causa-data cases verified by full contract validation and replay to the catalog's state digests | PASS (4/4) |
| Live persistence suite against the GitHub test repository | PASS (14/14) |
| Deploy + exact-SHA smoke | PASS |
| Deployed acceptance (live GitHub, Chromium + WebKit) | **FAIL**: 38 passed, 1 failed, 9 not run (§3.1) |
| PAT-13 provenance comparison against Rehearsals 1–6 | **BLOCKED** for this agent (KD-009) |

### 3.1 The deployed-acceptance failure

- **The failure:** in WebKit PAT-05, the test clicked Reapply before the conflict's GitHub version had been read.
- **App behaviour:** correct. The app answered that the version was still being read, and the screen showed it loaded shortly after.
- **Fix:** the test now waits for the review to show that version's commit. It is in #28, which is unmerged.
- **Consequence:** WebKit stopped after the failure, so 9 tests did not run, including PAT-11 (the same tests without Web Locks), PAT-07 and PAT-09. Those were therefore not verified live in WebKit on the final head. They pass in Chromium on the same run, and in both browsers against the fake.
- **The concurrent-save test now passes:** PAT-02, three tabs saving at once, passes live in both browsers. It had failed in every earlier deployed run until KD-022.

## 4. Deliverables

| Deliverable | Where | State at deadline |
|---|---|---|
| Built and deployed app | GitHub Pages at 1298489 | done |
| Reproducible build instructions | `README.md`, `global.json`, `package-lock.json`, `build.fsx` | done (README correction in #27) |
| Praxis work-item ledger | `evidence/ledger/work-items.json` (R7-WI-01..08) | done; statuses updated after the deadline |
| Traceability report | `evidence/traceability.json` | recorded at #24 (73 VERIFIED, 17 PARTIAL, 32 NOT_IMPLEMENTED, 9 NOT_RUN, 30 NOT_ASSESSED of 161 P0); **not re-assessed** after the review fixes |
| Test, security, accessibility, deployment evidence | `evidence/acceptance/ledger.json`, CI artifacts `core-evidence-*`, `live-evidence-*`, `deployed-evidence-*` | ledger recorded at #24; final runs listed in §3 |
| Provenance report | `evidence/provenance.md` | done |
| Token/cost telemetry | `evidence/telemetry/` | written after the deadline |
| Defect and remediation summary | `evidence/defects.md`, `evidence/DECISIONS.md` (KD-001..023 on main; KD-024 in #28) | done; third review in #28 |
| Independent acceptance verdict | §1 | three independent FAIL verdicts, reported as given |

## 5. Defects and remediation

Every fix below has a regression test. Each test was mutation-checked by reverting the fix in place and confirming that a test fails. Full tables are in `evidence/defects.md` (on #28 for the third review).

**Found by evidence during the build:**
- **KD-016:** CI could not fail, because steps ran without `pipefail`.
- **KD-017 / KD-019:** the code was built against the candidate data contract, not the frozen one. Found by the live catalog check.
- **KD-018:** an own commit whose answer was lost could be reported as a conflict.
- **KD-020:** secondary rate limits were reported as permission refusals.
- **KD-022:** GitHub's moved-head race (`FORBIDDEN: is at <oid> but expected <oid>`) was reported as a permission refusal. Found by a CI probe of three simultaneous commits.
- **CI writers overlapped:** live CI jobs from different runs wrote to the same test repository at once. They are now serialized across runs.

**Found by the independent reviews (fixed):**
- Saved could be shown without a read-back confirmation.
- A refresh could discard edits.
- The reader accepted forged or incomplete snapshots.
- Write markers were reused after a disconnect.
- A queued save could be sent while known offline.
- The credential could be sent to plaintext localhost.
- A conflict could get stuck without a way out.
- Contract-fixed values were not checked.
- A conflict re-read race could overwrite another writer's change (#28).
- A stale refresh could roll the tab back (#28).

**Open at the deadline:**
1. **Public protocol surface (reviews 2 and 3).** `SaveProcess`, `Load` and `Reply` are public, so in-process code that fabricates GitHub answers can reach a confirmation. No production path does this.
2. **Re-authentication.** A revoked or expired token offers only Reconnect, which discards the tab's unsaved work after a confirmation prompt.
3. **Unresolvable conflict.** A deleted or unverifiable GitHub version leaves the conflict without a resolution. The work is kept but cannot be saved.
4. **Smaller open items:**
   - compare results are not paginated;
   - files over 1 MB cannot be read back;
   - "Last saved" shows the confirmation time, not the commit time;
   - five low-severity wording and coverage items;
   - one test gap: removing the `eventsSha256` check is not caught.

## 6. Process notes

- **The deadline was missed for a process reason.** PR #28 went green at 03:51Z. This session then sat idle, and when the owner next wrote, the agent projected the time from earlier timestamps instead of reading the clock. It told the owner there were about two hours left when the window was in fact closing. The deadline passed with #28 unmerged and the evidence PR incomplete.
  - **Lesson:** read the wall clock at every resumption, and merge green work immediately.
- **Fixes can introduce defects.** The second review's fix for a stuck conflict introduced the race the third review found. Each review round found real defects that the author's own suites, including the mutation checks, had missed.
- **Live probes found what the fakes could not.** The two moved-head and rate-limit misclassifications only surfaced against real GitHub.

## 7. Work done after the deadline, at the owner's request (not part of the timed result)

- **CSS token fix (3d95108, PR #28).** A UI check found that four Forma token names in `web/causa.css` did not exist. As a result, five properties fell back to browser system colours and fonts:
  - page background;
  - accent;
  - strong borders;
  - body font.

  They now use Forma's real tokens, and light and dark modes follow the theme.
- **UI observations from that check, not fixed:**
  - the save-state badge is the smallest text on the page;
  - "Save to GitHub" is not visually distinct from other buttons;
  - the investigation page is one long column of always-open forms.
- **Evidence written after the deadline:** this report, the telemetry report, the ledger statuses and the checkpoint.

## 8. Telemetry summary

Full details are in `evidence/telemetry/README.md`.

| Measure | All agents, from T0 |
|---|---|
| Messages | 656 |
| Output tokens | 607,478 |
| Input tokens | 1,377 |
| Cache-read tokens | 258,140,455 |
| Cache-write tokens | 2,143,747 |
| Tool calls | 638 |

- **Agents included:** the main agent and the three independent reviewers.
- **Window:** these figures include about 3 minutes of post-deadline work.
- **Cost:** not computed, because no authoritative price list was available.

## 9. Recommendations

1. **Run an independent acceptance before any release claim.** A human or separate verifier should re-run acceptance on the final SHA, including the PAT-13 provenance comparison.
2. **Merge PR #28 and re-run the deployed acceptance.** This lands the third review's fixes and lets WebKit's PAT-07, PAT-09 and PAT-11 run live.
3. **Close the public protocol surface.** Move Limen's HTTP result decoding into Causa.Persistence so that `Reply` cannot be built outside it. Then add a PAT-12 negative project that drives `SaveProcess` with forged replies.
4. **Add re-authentication that keeps work.** Give a deleted or unverifiable GitHub version a resolution: re-create the investigation from the tab, or export the work.
5. **Paginate compare. Use commit times for "Last saved". Read large files through the Git blobs API.**
6. **Put a ruleset on `main`.** Require `build-and-test`, `independent-review` and `causa-ci`, which turns the advisory merge gate (KD-003) into a real one.
7. **Give each CI run its own live test branch,** instead of serializing every run on one.
8. **Schedule an independent adversarial review after every fix round, not only at the end.** Read the wall clock before any time-based decision.

## Addendum: after the deadline (2026-10-30T06:45Z)

Everything in this addendum happened after the 05:49:55Z deadline, at the owner's request. **None of it changes the timed result or the acceptance verdict.**

### Merged after the deadline

| PR | Content | Merge commit |
|---|---|---|
| #27 | this report, telemetry, ledger statuses, checkpoint, provenance | 02641fb |
| #28 | third-review fixes (conflict re-read race, stale refresh), PAT-05 test timing, Forma token CSS fix | 9b307aa |

### Main 9b307aa, run 41012468819: all jobs green

This was the first fully green `main` run of the rehearsal.

| Check | Result |
|---|---|
| Core: F# suites, PAT-12 negative, PAT-13 artifact scan, browser acceptance in Chromium and WebKit against the fake | PASS |
| Live provider: catalog 4/4 verified by replay, live persistence suite against GitHub | PASS |
| Deploy, with exact-SHA smoke | PASS |
| Deployed acceptance against live GitHub, Chromium and WebKit | **PASS, 48/48** |

What this resolves from §3.1:
- **WebKit PAT-05 passes.**
- **The 9 WebKit tests that did not run before now ran and passed:** the PAT-11 runs without Web Locks, PAT-07, PAT-09, PERSIST-020, EXT-DATA-002 and EXT-DATA-009.
- **The deployed site carries the CSS token fix** (§7) **and the third review's fixes.**

### What this addendum does not change

- **Acceptance remains NOT VERIFIED, NOT ACCEPTED.**
  - Three independent reviews each returned FAIL.
  - No independent review has examined 9b307aa.
  - The open items in §5 remain open.
- **The PAT-13 provenance comparison remains BLOCKED** for this agent.
- **Recommendation 1 stands:** an independent verifier should re-run acceptance on the final SHA, now 9b307aa.
