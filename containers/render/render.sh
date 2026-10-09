#!/bin/sh
# Render the chezmoi source (copied to /src) into /rendered as it would land in ~,
# for every path any kc image bakes. No run_ script, external or encrypted file is
# ever executed or fetched — this is `chezmoi archive`, not `chezmoi apply`.
set -eu

set -a
# shellcheck disable=SC1091
. /src/containers/render/container.env
set +a
export CHEZMOI_PROFILE="${KC_PROFILE:-personal}"

# chezmoi derives .chezmoi.homeDir from $HOME. Builds are always local, so render
# against the host's real home path: baked paths then match the bind mount.
export HOME="$KC_HOME"
mkdir -p "$HOME" /rendered

chezmoi execute-template --source /src < /src/.chezmoi.toml.tmpl > /tmp/chezmoi.toml

# Union of every TOOL_CONFIG_PATHS plus the toolbox shell layer. A path that the
# container render excludes (via .chezmoiignore) is skipped here and reported by
# kc-bake only if some tool actually asks for it.
targets=""
for p in .config/nvim .config/fish .config/starship.toml .config/nushell \
         .gitconfig .gitignore_global .tmux.conf; do
    targets="$targets $HOME/$p"
done

# shellcheck disable=SC2086
chezmoi --config /tmp/chezmoi.toml --source /src --destination "$HOME" \
    archive --format tar --exclude scripts,externals,encrypted \
    --output /tmp/rendered.tar $targets 2>&1 | grep -v 'not in source state' || true
tar -xf /tmp/rendered.tar -C /rendered

# lazy-lock.json is deliberately in .chezmoiignore (it churns on the host), so it
# never appears in rendered output. Images pin plugins to the committed copy.
if [ -d /rendered/.config/nvim ] && [ -f /src/dot_config/nvim/lazy-lock.json ]; then
    cp /src/dot_config/nvim/lazy-lock.json /rendered/.config/nvim/lazy-lock.json
fi

echo "rendered:"
find /rendered -maxdepth 3 | sort
