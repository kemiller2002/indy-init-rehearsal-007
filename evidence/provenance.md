# Provenance report (Rehearsal 7)

## Source authorship

- All application, test and tooling source in this repository was written during the timed run (T0 2026-10-29T22:49:55Z) by this agent in one Claude Code session. It was not copied from earlier rehearsals or from the existing Causa application. This agent did not access either of those; the mission forbids it.
- History on `main`:

  | Commit | Content |
  |---|---|
  | 5e33205 | Kickoff (the owner's starting state, DEC-R7-012) |
  | a8e2040 (#23) | Application, tests and CI |
  | 4657477 (#24) | Live hardening |
  | 5b70660 (#25) | First review remediation |

  The branch for #26 adds the concurrent-commit classification and the second review's remediation.
- Every commit carries a `Work-Item:` trailer (R7-WI-01 to R7-WI-08, `evidence/ledger/work-items.json`) and the session attribution trailer.
- **PAT-13 comparison with Rehearsals 1–6: BLOCKED for this agent (KD-009).**
  - The check is implemented as `dotnet fsi tools/acceptance.fsx provenance <candidate> <prior checkouts...>`. It reports any candidate source file over 20 lines that is byte-identical to a prior file.
  - It must be run by someone permitted to check out the prior rehearsals.

## Toolchain

| Component | Version | Pinned by |
|---|---|---|
| .NET SDK | 10.0.402 (roll-forward disabled) | `global.json` |
| wasm-tools workload | from the SDK feed | CI `dotnet workload install wasm-tools` |
| F# build script | `dotnet fsi build.fsx` (no FAKE, KD-008) | repository |

## Dependencies (exact versions, registry and integrity from `package-lock.json`)

| Package | Version | Source | Integrity (prefix) |
|---|---|---|---|
| @echelon-foundry/limen | 0.9.1 | registry.npmjs.org | sha512-9lKFnMmRuNjpn5PapSW0frO… |
| @echelon-foundry/design-system (Forma) | 0.12.0 | registry.npmjs.org | sha512-ZkXx1ofLo2EOewaq47Yo/Li… |
| @playwright/test / playwright / playwright-core | 1.63.0 | registry.npmjs.org | sha512-oxMK4vllB9RK5NQ2l1pq1If… |
| @axe-core/playwright | 4.10.2 | registry.npmjs.org | sha512-6/b5BJjG6hDaRNtgzLIfKr5… |
| axe-core (transitive) | 4.10.3 | registry.npmjs.org | sha512-Xm7bpRXnDSX2YE2YFfBk2Fn… |
| Expecto (NuGet, tests only) | 10.2.3 | nuget.org | NuGet package hash |

- CI installs with `npm ci` from the lockfile.
- The site ships only the 12 Limen modules reachable from the entry points (KD-011) and Forma's `all.css`. The PAT-13 artifact scan lists every shipped file.

## Build provenance

- `build.fsx` writes `build.json` with `GITHUB_SHA` into the site.
- The deploy job's smoke step checks that the served `build.json` equals the commit being deployed.
- `deployed-acceptance` runs against that exact deployed SHA.

## Data provenance

- The live catalog check (`tools/catalog.fsx`) reads `kemiller2002/causa-data` read-only in CI. Its output is limited to paths, shapes, versions and digests.
- It verified all 4 catalog cases by full contract validation and replay to the catalog's state digests (for example run 40993375765).
