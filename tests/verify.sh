#!/usr/bin/env bash
# Verification for the disposable autonomy readiness branch. A candidate that
# adds e2e/fail-ci fails it on purpose (the "failed CI" negative test).
set -euo pipefail
test -f README.md
if [ -e e2e/fail-ci ]; then
  echo "e2e/fail-ci is present: failing on purpose" >&2
  exit 1
fi
echo "verification passed"
