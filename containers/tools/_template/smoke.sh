#!/bin/sh
# Smoke test for kc-__TOOL__. Runs via `kc test __TOOL__` with --network none, so
# it also proves the tool works offline. Exit 0 = pass.
set -eu
__TOOL__ --version
