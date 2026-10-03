# dotfiles

My personal dotfiles managed with [chezmoi](https://www.chezmoi.io/).

## Quick Start

### Initial Setup

1. **Install chezmoi** (if not already installed):
   ```bash
   # macOS
   brew install chezmoi

   # Arch Linux
   sudo pacman -S chezmoi
   ```

2. **Initialize dotfiles**:
   ```bash
   chezmoi init https://github.com/Kirizan/dotfiles.git
   ```

3. **Review changes**:
   ```bash
   chezmoi diff
   ```

4. **Apply dotfiles**:
   ```bash
   chezmoi apply
   ```

### Post-Installation Setup

#### GitHub CLI Authentication

After applying dotfiles, authenticate GitHub CLI for full functionality:

```bash
gh auth login
```

Follow the prompts to:
- Select **GitHub.com**
- Choose **HTTPS** for Git operations protocol
- Authenticate via **web browser** (recommended) or **paste an authentication token**

Verify authentication:
```bash
gh auth status
```

#### Shell Configuration

If switching shells or on a new machine:

```bash
# Set zsh as default (macOS default)
chsh -s $(which zsh)

# Or set bash as default (Linux common)
chsh -s $(which bash)
```

Restart your terminal for changes to take effect.

## Updating Dotfiles

### Pull and Apply Changes

```bash
chezmoi update
```

### Edit a Dotfile

```bash
chezmoi edit ~/.bashrc
# Or apply immediately after editing
chezmoi edit --apply ~/.bashrc
```

### Add a New File

```bash
chezmoi add ~/.my-new-config
```

## Project Structure

```
~/.local/share/chezmoi/          # Source directory
├── .chezmoi.toml.tmpl           # Configuration with templates
├── .chezmoiignore               # Files to ignore
├── dot_bashrc.tmpl              # Templates ~/.bashrc
├── dot_config/                  # Templates ~/.config/
├── run_once_*.sh.tmpl           # Setup scripts (run once)
└── run_onchange_*.sh.tmpl       # Scripts run on changes
```

## Platform-Specific Configuration

Dotfiles automatically adapt to the current platform (macOS, Arch Linux, etc.) using chezmoi templates. See `CLAUDE.md` for detailed information on cross-platform configuration.

### SteamOS (Steam Deck / Steam Machine)

SteamOS is detected as `isSteamOS`, and `isArchBased` is then deliberately forced
**false** — see the comment in `.chezmoi.toml.tmpl`. SteamOS is Arch underneath, but
`isArchBased` is used throughout this repo to mean *"pacman is a safe way to install
things here"*, which it is not: the root filesystem is read-only and OS updates are
atomic A/B image swaps that discard everything under `/usr`. Forcing the flag off
neutralizes every `.isArchBased`-gated script at once, including ones added later.

What `chezmoi apply` deploys there is deliberately narrow — shell, git, tmux, editor
and Claude config. No packages, no desktop layer, no Obsidian. Durable software on
SteamOS belongs in flatpak, `~/.local/bin`, or a distrobox container.

Bootstrap:

```bash
# At the machine (Desktop Mode -> Konsole) — the only hands-on step
sudo systemctl enable --now sshd && ip -4 addr show scope global | grep inet

# chezmoi into ~/.local/bin, NOT via pacman (an OS update would wipe it)
sh -c "$(curl -fsLS get.chezmoi.io)" -- -b ~/.local/bin
chezmoi init --apply https://github.com/Kirizan/dotfiles.git
```

### Remote management from mimir

Enabling remote access is an **explicit opt-in**, never a side effect of
`chezmoi apply`:

```bash
cd ~/.local/share/chezmoi
make remote-status    # report current state, change nothing
make enable-remote    # sshd + mimir's public key + NOPASSWD sudo
make harden-remote    # disable SSH password auth (only after key auth is verified)
make disable-remote   # reverse enable-remote entirely
```

**Re-run `make enable-remote` after every SteamOS update.** An atomic image swap
discards `/usr` and can revert `/etc`, including the sudoers drop-in. The script is
idempotent, so every already-correct step reports as a skip.

Nothing secret is committed for this. mimir's SSH **public** key is embedded in
`scripts/steamos-remote-access.sh` — public keys are publishable by design. The
OpenBao SSH CA key is fetched at apply time rather than stored, and the machine's
account password lives only in OpenBao at `kv/infra/steam_machine`. No deploy key or
token is needed to bootstrap, since this repo is public and clones over HTTPS.

## Common Tasks

### View Current Configuration
```bash
chezmoi data
```

### Check for Differences
```bash
chezmoi diff
```

### Verify Dotfiles
```bash
chezmoi doctor
```

### Re-run Setup Scripts
```bash
chezmoi state delete-bucket --bucket=scriptState
chezmoi apply
```

## Troubleshooting

### Scripts Not Running
If `run_once_` scripts didn't execute:
```bash
chezmoi state delete-bucket --bucket=scriptState
chezmoi apply --force
```

### Authentication Issues
Re-authenticate GitHub CLI:
```bash
gh auth logout
gh auth login
```

### Merge Conflicts
If chezmoi detects conflicts:
```bash
chezmoi merge ~/.config/file
```

## More Information

- **Cross-platform configuration guide**: See `CLAUDE.md`
- **Chezmoi documentation**: https://www.chezmoi.io/
- **Report issues**: https://github.com/Kirizan/dotfiles/issues
