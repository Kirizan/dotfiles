# kc-agents (stub, not built)

Planned image for AI coding agents (Claude Code, OpenCode). It's kept out of the
toolbox on purpose: agents need auth tokens and network access, and the toolbox
should stay offline-safe.

Nothing here is built yet. These decisions are open:

1. **Auth storage.** Claude Code keeps credentials and settings in `~/.claude*`.
   Through the home mount that means the *host's* files, which contradicts the
   "never read host dotfiles" rule. The alternatives are the state volume (log in
   once per machine) or an explicit `--host-config`-style opt-in.
2. **Install method.** Claude Code's native installer writes to `~/.local`, so it
   would need a build-time `HOME`, with the binary copied into `/opt/kc`.
   Alternatively use the npm package.
3. **Network.** `TOOL_NETWORK=default`, so agents always need the network. That's
   fine, but it means agents are excluded from the offline smoke-test contract.

When this is ready, fill in `tool.conf`, `install.sh` and `smoke.sh`, then
`kc build agents`. Don't add it to `toolbox/tools.list`.
