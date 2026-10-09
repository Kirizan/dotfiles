# kc-tmux

tmux on its own, with the dotfiles config and TPM plugins baked into `/opt/kc/data/tmux/plugins`. Sessions live as long as the container, and tmux-resurrect saves to the `kc-tmux-state` volume. Not in `toolbox/tools.list`, because the toolbox shell layer already ships tmux.

## Template contract

Every item must hold before a tool goes into `toolbox/tools.list`.

- [ ] The Containerfile is built `FROM ${KC_BASE}` and ends with
      `ENTRYPOINT ["/usr/bin/tini", "--", "/usr/local/bin/kc-entrypoint", "<TOOL_CMD>"]`.
- [ ] Everything is installed with `apk` (`TOOL_APK`). Anything else is a pinned
      upstream binary with a SHA256 checksum in `install.sh`.
- [ ] Network access happens at build time only. The tool works offline at runtime
      (`kc test` runs it with `--network none`).
- [ ] The image never writes to host `~` except files the user edits. Config lives
      in `/opt/kc/config`, plugins and data in `/opt/kc/data`, state in the
      `kc-<tool>-state` volume.
- [ ] `smoke.sh` exits 0 when it passes.
- [ ] `org.kirizan.kc.*` labels come from `tool.conf` via `kc build`, never by hand.
- [ ] Builds for the native arch only (no multi-arch manifests).

## Build and test

```bash
kc build tmux
kc test tmux
kc run tmux --help
```
