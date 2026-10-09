#!/bin/sh
# Build-time hook for kc-nvim. Runs as root after TOOL_APK is installed and the
# rendered .config/nvim (plus the committed lazy-lock.json) is baked into
# /opt/kc/config/nvim. Network is allowed here and nowhere else: the result must
# work with --network none.
set -eu

# --- pinned upstream binaries (not in Wolfi) --------------------------------
# Checksums are the release-asset digests GitHub publishes for each file.
TYPST_VERSION=v0.15.1
TREE_SITTER_VERSION=v0.27.1

case $(uname -m) in
    x86_64)
        TYPST_TRIPLE=x86_64-unknown-linux-musl
        TYPST_SHA256=a6d077d0a95eed5a2eba715b2dae06be954f624ccbf85758a03f389ded33118c
        TS_ASSET=tree-sitter-linux-x64.gz
        TS_SHA256=d01042f94ba3d6827ddc9788e235f75d8366431be42bdf90ade534d960c98b7d ;;
    aarch64)
        TYPST_TRIPLE=aarch64-unknown-linux-musl
        TYPST_SHA256=5aa8d74a3d906e60ea12a66ac2f37f8eef1b14cbad7182a745e393a10c23dcee
        TS_ASSET=tree-sitter-linux-arm64.gz
        TS_SHA256=404be5d54a4955571c4067c21d4699660eab3a534cefb0659341b207f7139ec5 ;;
    *) echo "install.sh: unsupported arch $(uname -m)" >&2; exit 1 ;;
esac

fetch() { # fetch <url> <sha256> <out>
    curl -fsSL --retry 3 -o "$3" "$1"
    echo "$2  $3" | sha256sum -c -
}

tmp=$(mktemp -d)
fetch "https://github.com/typst/typst/releases/download/$TYPST_VERSION/typst-$TYPST_TRIPLE.tar.xz" \
      "$TYPST_SHA256" "$tmp/typst.tar.xz"
tar -xJf "$tmp/typst.tar.xz" -C "$tmp"
install -m 0755 "$tmp/typst-$TYPST_TRIPLE/typst" /usr/local/bin/typst

fetch "https://github.com/tree-sitter/tree-sitter/releases/download/$TREE_SITTER_VERSION/$TS_ASSET" \
      "$TS_SHA256" "$tmp/tree-sitter.gz"
gunzip -c "$tmp/tree-sitter.gz" >/usr/local/bin/tree-sitter
chmod 0755 /usr/local/bin/tree-sitter
rm -rf "$tmp"
typst --version
tree-sitter --version

# ltex-ls from Mason is the plain build (no bundled JRE), so it needs a system
# Java. Java 21 also predates the jdk.xml entity limit that breaks LanguageTool.
if [ "${KC_NVIM_LTEX:-true}" != false ]; then
    apk add --no-cache openjdk-21-jre openjdk-21-default-jvm
fi

# --- plugins, parsers, LSPs -------------------------------------------------
# HOME is /opt/kc/build-home (set by kc-tool-install). State and cache go to a
# throwaway dir so nothing build-time lands in the image's state mount point.
# KC_NVIM_BUILD: install plugins whose `cond` depends on the runtime machine
# (e.g. obsidian.nvim only loads where a vault exists).
export KC_NVIM_BUILD=1
export XDG_STATE_HOME=/tmp/kc-nvim-build/state
export XDG_CACHE_HOME=/tmp/kc-nvim-build/cache
mkdir -p "$XDG_STATE_HOME" "$XDG_CACHE_HOME"

nv() { nvim --headless "$@" 2>&1; }

echo "==> lazy: install + restore to lazy-lock.json"
nv "+Lazy! restore" +qa
# A second pass catches plugins whose build step needed another plugin present.
nv "+Lazy! restore" +qa

# markdown-preview's build hook downloads its server binary asynchronously and
# loses the race with +qa; run its installer synchronously instead.
mp=/opt/kc/data/nvim/lazy/markdown-preview.nvim/app
if [ -f "$mp/install.sh" ]; then
    (cd "$mp" && bash ./install.sh)
fi

echo "==> treesitter parsers"
nv "+luafile /opt/kc/tools/nvim/ts-install.lua"

echo "==> mason packages"
# LazyVim starts Mason installs on every startup; the phases above quit nvim
# mid-install, which can leave half-written staging dirs behind.
rm -rf /opt/kc/data/nvim/mason/staging
nv "+luafile /opt/kc/tools/nvim/mason-install.lua" || {
    echo "---- mason.log (tail)" >&2
    tail -60 "$XDG_STATE_HOME/nvim/mason.log" >&2 || true
    exit 1
}

# Lazy rewrites the lockfile on restore; keep the committed copy authoritative.
cp /opt/kc/config/nvim/lazy-lock.json /opt/kc/tools/nvim/lazy-lock.baked.json

rm -rf /tmp/kc-nvim-build
