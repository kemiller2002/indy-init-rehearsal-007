# Causa (Indy Init Rehearsal 7)

Causa is a root-cause investigation workspace. Investigations live in a GitHub repository you connect;
each browser tab works on its own in-memory copy and saves with one atomic, expected-head commit that is
confirmed by reading it back. Built from the frozen Indy-init requirements baseline
`BL-CAUSA-INDY-INIT-8445ca11986f` during the Rehearsal 7 timed run (see `evidence/kickoff/kickoff.json`).

## Architecture

| Layer | Where | What |
|---|---|---|
| Domain (F#) | `src/Causa.Domain` | JSON and RFC 8785 canonical form, the v1.0.0 event registry, `decide`/`evolve`, file-set verification |
| Persistence (F#) | `src/Causa.Persistence` | the seven-state save machine, GitHub requests and typed answer classification, the save process |
| Application (F#) | `src/Causa.App` | the Limen engine: model, update, view projection |
| Host (C#, 10 lines) | `src/Causa.Host` | the single `[JSExport]` entry point (F# cannot run the JSExport generator; decision KD-007) |
| Page | `web/` | HTML bound by Limen's `data-*` vocabulary, Forma CSS, and `main.js` (kernel start-up, connectivity, timers, leave warning) |

Published Echelon packages: `@echelon-foundry/limen` 0.9.1 and `@echelon-foundry/design-system` (Forma) 0.12.0.

## Build and test

Requirements: .NET SDK 10.0.402 (`global.json`) with the `wasm-tools` workload, and Node 22.

```sh
dotnet workload install wasm-tools
npm ci
dotnet fsi build.fsx test          # domain, PAT-12, PAT-14 and PAT-02..10 against the stateful GitHub fake
dotnet fsi build.fsx site          # publishes the WebAssembly engine and assembles dist/site
dotnet fsi tools/acceptance.fsx negative           # PAT-12 compile-time check
dotnet fsi tools/acceptance.fsx artifact dist/site # PAT-13 artifact scan
dotnet build tests/Causa.TestServer -c Release
npx playwright install chromium webkit
npx playwright test                # PAT-01/02/04/05/07/09/11/15 in Chromium and WebKit against the fake
```

Run the app locally against the fake: `dotnet tests/Causa.TestServer/bin/Release/net10.0/Causa.TestServer.dll dist/site 4300`,
open http://127.0.0.1:4300, and connect to owner `causa-test`, repository `data`, any workspace folder, token `test-token`,
with the advanced API address `/github` (the same-origin test stand-in; the app sends the credential only to `https://api.github.com` or its own origin). The page shows that it is using a test provider.

Live provider tests run when `CAUSA_WRITE_TOKEN` is set (write access to the test repository only);
the live catalog check uses the read-only `CAUSA_GITHUB_TOKEN`. Without them the result is BLOCKED, never PASS.
