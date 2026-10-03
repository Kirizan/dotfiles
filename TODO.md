# TODO

Planned work for this dotfiles repo that isn't a bug. Hardware and driver bugs
with deployed workarounds belong in `KNOWN_BUGS.md` instead.

Entries are numbered so commits and skills can reference them. Remove an entry
when the work ships.

---

## TODO-001: Tailscale network switching and shutdown from the application menu

- **Status:** Open
- **Affects:** Graphical machines (currently mimir)
- **Related:** `dot_local/share/applications/kirizan-tailscale.desktop.tmpl`,
  `dot_local/bin/executable_tailscale-toggle`
- **Date added:** 2026-09-17

Add application-menu entries for switching between Tailscale networks and for
shutting Tailscale down, so neither requires a terminal.

Today the launcher exposes Start / Stop / Show status as desktop *actions*,
which in KDE are only reachable by right-clicking the entry. Switching accounts
isn't exposed at all — it needs `sudo tailscale switch <id>` by hand.

### What's wanted

1. **Switch networks** — pick a tailnet profile from the menu. Profiles on mimir
   today:

   | ID | Tailnet | Purpose |
   |------|----------------------------------|------------------------------|
   | `9cad` | `headscale.internal.kirby.network` | Self-hosted Headscale — Minecraft |
   | `5c05` | `lowcoordination.github` | Friend's tailnet |

2. **Close Tailscale** — a first-class icon rather than a right-click action.

### Implementation notes

- A static `.desktop` file can't enumerate profiles. Either generate one entry
  per profile from a chezmoi run script (stale when profiles change), or ship a
  single "Switch Tailscale Network" entry that shells out to a chooser —
  `kdialog --menu` fits the KDE stack and can be fed live from
  `tailscale switch --list --json`.
- Profile commands need root: `tailscale switch --list` fails as the user with
  "profiles access denied". Either keep using the `as_root` helper already in
  `tailscale-toggle`, or run `sudo tailscale set --operator=$USER` once and drop
  the escalation entirely. The operator route is cleaner if more of this moves
  into the menu.
- Switching while the daemon is stopped is meaningless — have the switch action
  start the daemon first, reusing `tailscale-toggle start` so the existing
  BackendState wait and notifications are not duplicated.
- `tailscale switch` is still marked **alpha** by the CLI; expect the interface
  to move.
- Whichever profile is active is what the launcher brings up, so the menu should
  make the current tailnet obvious — the notification text is the cheap way.

---

## TODO-002: The OpenBao SSH CA is configured but unused

- **Status:** Open
- **Affects:** All Linux fleet members
- **Related:** `run_onchange_configure-ssh-ca.sh.tmpl`, `.chezmoi.toml.tmpl`
  (`data.openbao.ssh_ca_url`)
- **Date added:** 2026-10-03

`run_onchange_configure-ssh-ca.sh.tmpl` fetches the CA public key from
`https://vault.internal.kirby.network/v1/ssh/public_key` and writes
`TrustedUserCAKeys /etc/ssh/trusted-user-ca-keys.pem` into `sshd_config`, so every
host trusts certificates signed by that CA.

**But nothing in the fleet presents a certificate.** mimir has no
`~/.ssh/id_ed25519-cert.pub`, so all actual SSH access is plain public-key auth via
`authorized_keys`. The CA trust is inert — it grants nothing and protects nothing.

Discovered while onboarding the Steam Machine, where the CA looked like the tidy
way to grant access without committing a key, and turned out not to work.

### Two ways out, pick one

1. **Make it real.** Sign mimir's key from OpenBao's `ssh` mount, deploy the cert
   to `~/.ssh/`, and wire renewal (certs expire — that is the point). Then
   `authorized_keys` entries can be dropped fleet-wide and access is managed
   centrally by issuing and revoking certs.
2. **Remove it.** Delete the run script and the `ssh_ca_url` data key. Keep
   `authorized_keys`, which is what actually works today.

Option 1 is the better end state; option 2 is honest about what is deployed. What
exists now is the worst of both — the appearance of certificate auth without the
substance.

---

## TODO-003: Several `.chezmoiignore` entries silently never match

- **Status:** Open
- **Affects:** All machines; most visibly non-Arch ones
- **Related:** `.chezmoiignore`
- **Date added:** 2026-10-03

Scripts are matched in `.chezmoiignore` by their **target** name — the `run_`
prefix and `.tmpl` suffix are stripped first. Entries written in source form never
match anything and fail silently.

Confirmed wrong (source-form entries that match nothing):

- the `not .isArchBased` gaming block — `run_once_install-gaming-packages.sh.tmpl`,
  `run_once_after_install-battlenet.sh.tmpl`
- the `fleet-update-check` block — `run_onchange_after_enable-fleet-update-timer.sh.tmpl`
- the `isHexOS` block — most of its `run_*.sh.tmpl` entries, plus two written as
  `run_once_...sh` which keep the `run_once_` prefix and so also never match

The correct form is the bare target name, e.g. `install-gaming-packages.sh`,
`enable-fleet-update-timer.sh`. The opt-in block's `install-curseforge.sh` /
`install-minecraft.sh` entries are already written correctly and confirm the
convention.

Impact is currently masked: the affected scripts are also gated internally by
template conditionals, so they render as no-op stubs rather than running. The
ignore list is doing nothing, and the guards are doing all the work. Fixing the
names is low-risk but touches behaviour on the Spark and avalon, so it wants its
own change and its own verification rather than riding along with unrelated work.

The SteamOS block added alongside this entry already uses the correct form, and
carries a comment explaining the convention.
