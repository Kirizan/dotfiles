#!/bin/sh
# kc-entrypoint — runs as PID 2 under tini in every kc image.
#
# 1. passwd shim: make sure the running UID has a passwd entry. Rootless podman
#    with --userns=keep-id and the baked build user both already provide one; a
#    docker --user override with an unbaked UID does not, and tools like ssh, git
#    and fish misbehave without it.
# 2. Make sure the per-tool state volume has the XDG dirs we point at.
# 3. exec the requested command.
set -e

uid=$(id -u)
gid=$(id -g)
if ! getent passwd "$uid" >/dev/null 2>&1; then
    name=${USER:-kc}
    if getent passwd "$name" >/dev/null 2>&1; then name="kc$uid"; fi
    getent group "$gid" >/dev/null 2>&1 || echo "$name:x:$gid:" >>/etc/group
    echo "$name:x:$uid:$gid:kc user:${HOME:-/opt/kc/build-home}:/usr/bin/fish" >>/etc/passwd
fi

mkdir -p "$XDG_STATE_HOME" "$XDG_CACHE_HOME" 2>/dev/null || true

if [ $# -eq 0 ]; then
    set -- /bin/bash -l
fi
exec "$@"
