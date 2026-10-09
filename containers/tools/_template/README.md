# kc-__TOOL__

One-paragraph description of what this image is for.

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
kc build __TOOL__
kc test __TOOL__
kc run __TOOL__ --help
```
