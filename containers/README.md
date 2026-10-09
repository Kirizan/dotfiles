# kc: containerized dotfile toolchain

`kc` runs the dotfile CLI toolchain (fish, Neovim, tmux, git, fzf, rg and the rest)
from locally built OCI images. It's for two cases:

- **Machines where you can't install software**, such as the work laptop.
- **Guest mode.** You bring your toolchain to someone else's machine without
  chezmoi touching their `~/.config`, `~/.zshrc` and the like.

**The image holds the config, and `~` holds only your files.** The chezmoi source is
rendered while the image builds, and that config is baked in under `/opt/kc`. Host `~`
is bind-mounted at the same path so you can work on your files, but the containers
never read or write its dotfiles. Mutable tool state lives in one named volume per
tool, `kc-<tool>-state`.

The design record (goals, decisions, phase history) is the Obsidian note
*Containerized Dotfile Toolchain Plan*.

## Quick start

```bash
kc build all          # base → render → every tool in toolbox/tools.list → toolbox
kc test nvim          # offline smoke test (--network none)
kc run                # fish in the toolbox, in the current directory
kc run nvim notes.md  # one tool
kc doctor             # runtime, arch, SELinux, proxy, CA, image ages
```

On your own machines chezmoi deploys `~/.local/bin/kc` as a symlink into this repo.
On a guest machine, clone the repo anywhere and run `containers/bin/kc` directly.

Every machine builds its own images for its native arch. There is no registry, no
export/import and no cross-building. Images are rebuilt by hand (`kc build all`), and
`kc doctor` flags any image older than 30 days.

## Commands

| Command | What it does |
|---|---|
| `kc run [--ports] [--ssh] [--host-config] [--network X] [tool] [args]` | Run a tool. With no tool it runs the toolbox, which defaults to `fish -l` |
| `kc build [base\|render\|toolbox\|all\|<tool>...]` | Build images; `base` and `render` are built as needed |
| `kc test <tool\|toolbox>` | Run the image's `smoke.sh` with `--network none` |
| `kc shim install\|remove [tool]` | Write or remove `~/.local/bin/<shim>` wrappers, e.g. `nvim` → `kc run nvim` |
| `kc env [--fish]` | Print a PATH export for session-only shims in `~/.local/share/kc/bin` |
| `kc new <tool>` | Scaffold `tools/<tool>/` from `tools/_template/` |
| `kc list` / `kc doctor` / `kc clean [tool]` | List images, run diagnostics, remove state volumes |

The `make` targets (`make build TOOL=nvim`, `make test-all`, `make lint`) are thin
wrappers around `bin/kc`.

### Own machine vs guest machine

- **Own machine or work laptop:** run `kc shim install`. After that `nvim`,
  `EDITOR=nvim` and `git commit` all go through the container transparently. A shim
  never overwrites a file that isn't a kc shim.
- **Someone else's machine:** run `eval "$(kc env)"`, or `kc env --fish | source`. That
  shims only the current shell. Nothing outside `~/.local/share/kc` is written, and
  `kc clean` removes the state volumes.

### Home-only rules

- `kc run` refuses to start when `$PWD` is outside `$HOME`. With
  `KC_ALLOW_OUTSIDE=1` it starts in `~` instead.
- An absolute argument outside `$HOME` produces a warning. The common case is a host
  tool calling `$EDITOR` on a `/tmp` file, such as `crontab -e` or `sudoedit`. With
  `KC_ALLOW_OUTSIDE=1` that file's parent directory is mounted.
- **SSH agent:** a socket under `~`, such as 1Password's `~/.1password/agent.sock`, is
  forwarded automatically. Any other socket is forwarded only with `--ssh`.
- **Git identity is never baked.** Git reads the baked aliases and settings from
  `/opt/kc/config/git/config`, plus the host's `~/.gitconfig` through the home mount.
  Identity therefore comes from the machine you're on, or stays unset.

## Layout

```
containers/
├── bin/kc                  launcher (POSIX sh), the only runtime-specific code
├── Makefile                wrappers around bin/kc
├── base/                   kc-base: Wolfi + shared runtime + entrypoint/kc-bake/kc-tool-install
│   └── certs/              corporate CA certs land here at build time (gitignored)
├── render/                 kc-render: build-only stage, `chezmoi archive` → /rendered
├── tools/
│   ├── _template/          copied by `kc new`
│   ├── nvim/               kc-nvim (in tools.list)
│   ├── tmux/               kc-tmux (standalone; toolbox already has tmux)
│   └── agents/             stub, not built (see its README)
└── toolbox/                kc-toolbox: tools.list + shell layer (fish, starship, tmux…)
```

The build context is the repo root. `/.dockerignore` allowlists the chezmoi source
and `containers/`.

### Image layout

| Path | Contents |
|---|---|
| `/opt/kc/config` | `XDG_CONFIG_HOME`: rendered chezmoi config (baked) |
| `/opt/kc/data` | `XDG_DATA_HOME`: plugins, parsers, LSPs, tmux plugins (baked) |
| `/opt/kc/state` | Named volume `kc-<tool>-state`: `XDG_STATE_HOME` and `XDG_CACHE_HOME` |
| `/opt/kc/tools` | Tool definitions, including the smoke tests |

Builds are always local, so `kc build` bakes your user (name, UID, GID and home path)
into the image. Rendered paths then match the bind mount, and files you create under
`~` come out owned by you.

## How rendering works

`render/render.sh` runs `chezmoi archive`, never `apply`: no `run_` script,
external or encrypted file is touched. It renders with these overrides
(`render/container.env`):

- `CHEZMOI_CONTAINER=true` sets `.isContainer`. **This flag is opt-in only.**
  It's deliberately not detected from `/run/.containerenv`, because distrobox (the
  SteamOS install path) creates that file. Templates read it as
  `get . "isContainer"` so older persisted configs keep working.
- `HOME` is set to your real home path, so `.chezmoi.homeDir` matches the mount.

`lazy-lock.json` is in `.chezmoiignore`, so the render stage copies the committed
`dot_config/nvim/lazy-lock.json` instead. Refresh that file from
`~/.config/nvim/lazy-lock.json` whenever you want the images to pick up plugin updates.

`isContainer` changes these templates:

| File | Container behavior |
|---|---|
| `.chezmoiignore` | `.config/fish` renders even with no fish binary in the render stage |
| `dot_gitconfig.tmpl` | No `[user]`, no `includeIf`, no `excludesFile` (git reads `$XDG_CONFIG_HOME/git/ignore`) |
| `dot_config/fish/config.fish.tmpl` | No `go env -w` (the image sets `GOPROXY`), and never sources the host's `local.fish` |
| `dot_tmux.conf.tmpl` | OSC 52 clipboard; TPM plugins baked at `/opt/kc/data/tmux/plugins`; resurrect state in the volume |

The Neovim config checks `KC_IN_CONTAINER`, which `kc-base` sets, at runtime:

- `lazy.lua`: plugins clone over HTTPS, and the update checker is off.
- `container-clipboard.lua`: OSC 52.
- `typst.lua`: Podman is detected too.
- `grammar-checking.lua.tmpl`: ltex is skipped when `KC_NVIM_LTEX=false`.

## Runtimes

`kc` uses `KC_RUNTIME` if it's set, otherwise podman, otherwise docker.

| Platform | Runtime | User mapping | Notes |
|---|---|---|---|
| Linux + rootless podman | podman | `--userns=keep-id --user uid:gid` | `--user` is required: keep-id alone runs a USER-less image as container root |
| Linux + docker | docker | `--user uid:gid` + passwd shim | |
| macOS (work laptop) | Podman machine or Colima | `--user uid:gid` | Native arm64 on Wolfi. The VM's file sharing maps ownership |
| WSL | either | same as Linux | Clipboard is OSC 52 |
| Fedora/RHEL (SELinux) | podman | keep-id | `--security-opt label=disable`. **Never** `:z`/`:Z` on `$HOME` |
| SteamOS | podman | keep-id | Matches the distrobox/flatpak policy |
| DGX Spark | docker | `--user` | Native arm64 |

## Work and corporate networks

- **TLS-intercepting proxy:** put the corporate CA certs in `~/.config/kc/certs/*.crt`.
  `kc build` copies them into `base/certs/`, which is gitignored, and installs them into
  the image trust store. The image sets `SSL_CERT_FILE`, `NODE_EXTRA_CA_CERTS` and
  `REQUESTS_CA_BUNDLE`, so Lazy, Mason, npm and pip trust them.
- **Proxy env:** `HTTP(S)_PROXY` and `NO_PROXY`, in upper and lower case, are passed
  through at build time and at run time.

## Adding a tool

1. Run `kc new <tool>`.
2. Fill in `tools/<tool>/tool.conf`, plus `install.sh` and `smoke.sh` if needed.
3. Run `kc build <tool>`, then `kc test <tool>`.
4. Add the tool to `toolbox/tools.list` once it meets the contract in
   `tools/_template/README.md`.

`tool.conf` becomes `org.kirizan.kc.*` image labels, so `kc run` needs no per-tool code.

Tools outside `tools.list`:

- `tools/tmux/` is built and tested as a standalone image. The toolbox already
  ships tmux through its shell layer.
- `tools/agents/` is a stub and isn't built. Its auth, install method and network
  decisions are still open; see `tools/agents/README.md`.

## Known limitations

- **`Kirizan/typst-acr.nvim` isn't in the images.** It's a private repo and image
  builds have no GitHub credentials, so the plugin spec sets
  `cond = vim.env.KC_IN_CONTAINER == nil`.
- **Image size.** kc-nvim is about 3.3 GB. The C toolchain, Go, Node, the JRE for
  ltex and the Mason LSPs dominate. `KC_NVIM_LTEX=false` saves about 250 MB plus the
  JRE. Moving the compilers into a builder stage is the next lever.
- **Verified only on rootless podman on mimir (x86_64).** The docker path, macOS
  (Podman machine or Colima), WSL and arm64 (the Spark) haven't been run yet.
- **TPM plugins are cloned at HEAD at build time.** Only TPM itself is pinned
  (v3.1.0). Neovim plugins are pinned by `lazy-lock.json`.

## Phase 0 audit: Wolfi package availability (2026-10-08)

| Need | Wolfi package | Notes |
|---|---|---|
| bash, zsh, fish | `bash` 5.3, `zsh` 5.9, `fish` 4.9 | |
| neovim | `neovim` 0.12.5 | |
| git, lazygit, gh | `git` 2.56, `lazygit` 0.66, `gh` 2.102 | |
| ripgrep, fd, fzf | `ripgrep`, `fd`, `fzf` | |
| starship, direnv, eza, bat, jq, tree, wget, tmux | all present | `tmux` 3.8 |
| chezmoi | `chezmoi` 2.73 | |
| node, npm, python, go | `nodejs` (26), `npm`, `python-3` (3.13), `go` (1.27) | Unversioned names resolve to the newest |
| C toolchain | `build-base` | gcc + make + glibc-dev |
| aws-cli, openbao, nushell | `aws-cli-2`, `openbao`, `nushell` | |
| terraform | `terraform` **1.5.7** (last MPL release, frozen) | Use `opentofu` (1.11) |
| **tree-sitter CLI** | **missing** (`tree-sitter` is only the library) | Pinned upstream v0.27.1 + SHA256 |
| **typst** | **missing** | Pinned upstream v0.15.1 + SHA256 |
| tar | missing as its own package | busybox `tar` covers it |

Pinned checksums are the release-asset digests that GitHub publishes. They live in
`tools/nvim/install.sh`.
