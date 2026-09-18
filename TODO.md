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
