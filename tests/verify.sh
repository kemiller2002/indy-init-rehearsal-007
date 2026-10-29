#!/usr/bin/env bash
# Required CI check for Rehearsal 7 (build-and-test). Rehearsal work replaces
# or extends this with the frozen verification plan; changing it is
# REQUIRES_OWNER because CI runs it.
set -euo pipefail
test -f README.md
echo "verification passed"
