#!/bin/sh
# Smoke test for kc-agents. Runs via `kc test agents` with --network none, so
# it also proves the tool works offline. Exit 0 = pass.
set -eu
agents --version
