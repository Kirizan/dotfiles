#!/bin/sh
# Build-time hook for kc-agents. Runs as root inside the image build, after
# TOOL_APK is installed and TOOL_CONFIG_PATHS are baked into /opt/kc/config.
#
# Anything not in Wolfi: install the upstream release binary at a PINNED version
# with a SHA256 checksum (see tools/nvim/install.sh for the pattern).
set -eu
