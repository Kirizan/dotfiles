#!/bin/sh
# Smoke test for kc-tmux (kc test tmux, --network none).
set -eu
tmux -V
ls /opt/kc/data/tmux/plugins
n=$(find /opt/kc/data/tmux/plugins -mindepth 1 -maxdepth 1 -type d | wc -l)
[ "$n" -ge 6 ] || { echo "FAIL: expected 6 tmux plugins, found $n" >&2; exit 1; }
TMUX_TMPDIR=$(mktemp -d) tmux -f /opt/kc/config/tmux/tmux.conf new-session -d -s smoke \; kill-server
echo "kc-tmux smoke: PASS"
