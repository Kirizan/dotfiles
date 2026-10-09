#!/bin/sh
# Smoke test for kc-nvim. Run by `kc test nvim` with --network none, so passing
# also proves the plugins, parsers and LSPs are baked and work offline.
set -eu
here=/opt/kc/tools/nvim
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT

echo "--- nvim --version"
nvim --version | head -1
typst --version
tree-sitter --version

echo "--- every enabled plugin is installed (pinned by lazy-lock.json)"
locked=$(grep -c '"branch"' /opt/kc/config/nvim/lazy-lock.json)
echo "lazy-lock.json entries: $locked"
nvim --headless "+luafile $here/smoke-plugins.lua" 2>&1; echo

echo "--- checkhealth"
TERM=xterm-256color nvim --headless "+checkhealth" "+w! $work/health.txt" +qa >/dev/null 2>&1 || true
# Known-benign ERRORs are listed in smoke.allow, one fixed string per line. Each
# one reproduces in mimir's own headless checkhealth, so it's config/headless
# behavior, not an image defect:
#   snacks — its healthcheck indexes a highlight fg that's nil without a UI.
grep -n 'ERROR' "$work/health.txt" | grep -v -F -f "$here/smoke.allow" >"$work/errors.txt" || true
if [ -s "$work/errors.txt" ]; then
    echo "FAIL: checkhealth reported errors:" >&2
    cat "$work/errors.txt" >&2
    exit 1
fi
echo "checkhealth: no unexpected ERROR lines"

echo "--- LSP attach (markdown)"
cp "$here/fixtures/sample.md" "$here/fixtures/sample.typ" "$work/"
(cd "$work" && KC_SMOKE_CLIENTS="marksman markdown_oxide harper_ls" \
    nvim --headless --cmd "luafile $here/smoke.lua" sample.md 2>/dev/null)

echo "--- LSP attach (typst)"
clients=tinymist
[ "${KC_NVIM_LTEX:-true}" = false ] || [ "$(uname -m)" != x86_64 ] || clients="$clients ltex"
(cd "$work" && KC_SMOKE_CLIENTS="$clients" \
    nvim --headless --cmd "luafile $here/smoke.lua" sample.typ 2>/dev/null)

echo "kc-nvim smoke: PASS"
