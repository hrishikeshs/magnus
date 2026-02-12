#!/bin/bash
# Magnus permission handler for Claude Code PermissionRequest hooks.
#
# This script acts as a thin transport layer between CC and Emacs:
# 1. Reads the hook JSON from stdin, writes to a temp file
# 2. Notifies Emacs via emacsclient (non-blocking)
# 3. Polls for a response file written by Emacs
# 4. Outputs the response JSON to stdout for CC
#
# If Emacs is unavailable, exits 1 so CC falls back to its
# normal interactive permission dialog in the terminal.

set -euo pipefail

REQFILE=$(mktemp /tmp/magnus-perm-XXXXXX.json)
RESPFILE="${REQFILE%.json}.resp.json"

# Read hook input JSON from stdin
cat > "$REQFILE"

# Clean up temp files on exit
cleanup() { rm -f "$REQFILE" "$RESPFILE"; }
trap cleanup EXIT

# Notify Emacs (non-blocking) — pass both file paths
if ! emacsclient -n --eval \
  "(magnus-permission-notify \"$REQFILE\" \"$RESPFILE\")" >/dev/null 2>&1; then
  # Emacs server not available; fall back to normal CC dialog
  exit 1
fi

# Poll for response file (max ~600s = CC hook timeout)
for _ in $(seq 1 1200); do
  if [ -f "$RESPFILE" ]; then
    cat "$RESPFILE"
    exit 0
  fi
  sleep 0.5
done

# Timed out waiting for Emacs response
exit 1
