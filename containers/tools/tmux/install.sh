#!/bin/sh
# Build-time hook for kc-tmux: bake tmux plugins (TPM) into
# /opt/kc/data so tmux never clones anything into host ~ at runtime.
set -eu

TPM_VERSION=v3.1.0
plugins=/opt/kc/data/tmux/plugins
mkdir -p "$plugins"
git clone --quiet --depth 1 --branch "$TPM_VERSION" https://github.com/tmux-plugins/tpm "$plugins/tpm"

# install_plugins reads the plugin list from a running server's config.
export TMUX_PLUGIN_MANAGER_PATH=$plugins/
TMUX_TMPDIR=$(mktemp -d)
export TMUX_TMPDIR
tmux -f /opt/kc/config/tmux/tmux.conf new-session -d -s kc-build
"$plugins/tpm/bin/install_plugins"
tmux kill-server || true
rm -rf "$TMUX_TMPDIR"
ls "$plugins"
