# Token and cost telemetry (Rehearsal 7)

- **Source:** the provider's per-message `usage` records in the Claude Code session transcripts.
  - Records are read-only and de-duplicated by message id.
  - Tool: `dotnet fsi tools/telemetry.fsx <transcripts> 2026-10-29T22:49:55Z session_01G1iECdaTWsbm5P9ZJ1nuMG`.
- **Nothing is estimated.**

| Agent | Phase | Messages | Input | Output | Cache read | Cache write | Tool calls | Window |
|---|---|---|---|---|---|---|---|---|
| Main agent | preflight (before T0) | 46 | 92 | 25,303 | 6,983,199 | 147,880 | 55 | 22:37:56–22:49:54Z |
| Main agent | from T0 | 510 | 1,083 | 490,062 | 238,493,928 | 1,571,829 | 484 | 22:50:02–05:52:01Z |
| Independent reviewer 1 | timed | 48 | 96 | 40,193 | 6,647,307 | 197,797 | 51 | 02:05–02:16Z |
| Independent reviewer 2 | timed | 40 | 82 | 32,231 | 4,880,894 | 172,331 | 43 | 02:44–02:53Z |
| Independent reviewer 3 | timed | 58 | 116 | 44,992 | 8,118,326 | 201,790 | 60 | 03:17–03:30Z |
| **All agents, from T0** | | **656** | **1,377** | **607,478** | **258,140,455** | **2,143,747** | **638** | |

All of these ran on the same model (see the per-file `models` field).

Limits:
- **Main agent's "from T0" row:** it runs to the time the report was generated, 05:52Z. That is about 2 minutes past the 05:49:55Z deadline, so it includes this post-deadline write-up. The tool has no cut-off at the deadline, and the records were not split by hand.
- **Subagent transcripts:** these are separate files and are counted separately.
- **Not counted:** CI runner time, GitHub Actions minutes, and any token use outside these transcripts.
- **Cost:** not computed. No authoritative price list was available in this session, and estimating one would violate the no-estimates rule.

Raw outputs: `main-agent.json`, `reviewer-*.json`.
