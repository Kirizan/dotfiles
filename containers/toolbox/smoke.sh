#!/bin/sh
# Smoke test for kc-toolbox (kc test toolbox, --network none).
set -eu
echo "--- shell layer"
fish --version
starship --version | head -1
tmux -V
chezmoi --version
for b in direnv eza bat jq tree rg fd fzf git; do command -v "$b" >/dev/null || { echo "FAIL: $b missing" >&2; exit 1; }; done

echo "--- fish config loads cleanly and prompt renders"
out=$(fish -l -c 'starship prompt' 2>&1) || { echo "$out" >&2; exit 1; }
echo "$out" | head -2

echo "--- git: no baked identity"
email=$(git config --get user.email || true)
[ -z "$email" ] || { echo "FAIL: user.email baked as '$email'" >&2; exit 1; }
git config --get alias.dag >/dev/null || { echo "FAIL: aliases not baked" >&2; exit 1; }

echo "--- tmux plugins baked and config parses"
ls /opt/kc/data/tmux/plugins
n=$(find /opt/kc/data/tmux/plugins -mindepth 1 -maxdepth 1 -type d | wc -l)
[ "$n" -ge 6 ] || { echo "FAIL: expected 6 tmux plugins, found $n" >&2; exit 1; }
TMUX_TMPDIR=$(mktemp -d) tmux -f /opt/kc/config/tmux/tmux.conf new-session -d -s smoke \; kill-server

echo "--- every tool in tools.list"
for t in $(grep -vE '^\s*(#|$)' /opt/kc/toolbox/tools.list); do
    /opt/kc/tools/$t/smoke.sh
done
echo "kc-toolbox smoke: PASS"
