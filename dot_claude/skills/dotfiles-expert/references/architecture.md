# Repository Architecture Reference

## Complete File Inventory

### Root Configuration
- `.chezmoi.toml.tmpl` - Master config (~6,300 bytes): platform detection, data variables, package manager abstraction
- `.chezmoiignore` - Platform-conditional ignoring (~1,650 bytes)
- `.chezmoiremove` - Files to remove (currently: `.config/nvim/lua/plugins/spell-checking.lua`)
- `.chezmoiexternal.toml` - External git repos (Omarchy Catppuccin dark theme, weekly refresh)
- `Makefile` - Build/utility targets
- `README.md`, `CLAUDE.md`, `HOTKEYS.md`, `LICENSE` - Documentation
- `PROFILES.md.tmpl` - Profile documentation (rendered with data)
- `.gitignore` - Git ignore patterns

### Shell Configurations
| File | Shell | Platform | Features |
|------|-------|----------|----------|
| `dot_zshrc.tmpl` (~6,400 bytes) | zsh | macOS | Homebrew PATH, GNU tool aliases (eza, gsed, ggrep, gtar), fzf with Catppuccin Mocha, pyenv, direnv, Terraform/AWS completion, starship prompt, EDITOR/VISUAL=nvim |
| `dot_bashrc.tmpl` (~3,900 bytes) | bash | Linux | Omarchy integration, WSL clipboard (clip.exe), CUDA support, fzf, pyenv, direnv, starship prompt, EDITOR/VISUAL=nvim |
| `dot_bash_profile.tmpl` (~100 bytes) | bash | Linux | Sources .bashrc |
| `dot_zprofile` (34 bytes) | zsh | macOS | Static, emacs keybindings |
| `dot_config/fish/config.fish.tmpl` | fish | Both | Cross-platform, CUDA, Homebrew, WSL, work profiles, EDITOR/VISUAL=nvim |
| `dot_config/nushell/` | nushell | Both | Private config |

### Application Configurations
| Path | Application | Details |
|------|-------------|---------|
| `dot_config/nvim/` | Neovim (LazyVim) | 40+ plugin configs, grammar checking (LTeX-LS), Catppuccin theme, markdown/typst support |
| `dot_config/hypr/` | Hyprland | Wayland compositor, multiple config files, conditionally ignored if Hyprland not installed |
| `dot_config/waybar/` | Waybar | Status bar, conditionally ignored if waybar not installed |
| `dot_config/mako/` | Mako | Notification daemon, conditionally ignored if makoctl not installed |
| `dot_config/alacritty/alacritty.toml` | Alacritty | Terminal emulator config |
| `dot_config/wezterm/private_wezterm.lua` | WezTerm | Terminal (private, chmod 600) |
| `dot_config/starship.toml` | Starship | Cross-platform prompt theme |
| `dot_config/autostart/1password.desktop.tmpl` | 1Password | Desktop autostart entry |
| `dot_config/vim/gvimrc` | Vim GUI | GVim settings |
| `dot_tmux.conf.tmpl` (~6,300 bytes) | tmux | Catppuccin theme, TPM plugin manager, resurrect/continuum/sessionx plugins, vi-mode |
| `dot_gitconfig.tmpl` (~1,000 bytes) | Git | Profile-aware user/email, platform-specific credential helpers, personal/work includes |
| `dot_gitconfig_personal.tmpl` | Git | Personal profile git includes |
| `dot_ideavimrc` (~2,900 bytes) | JetBrains | IdeaVim keybindings |
| `dot_Brewfile.tmpl` (~4,100 bytes) | Homebrew | macOS packages: 60+ Nerd Fonts, GUI apps (Brave, Firefox, WezTerm), CLI tools, duti, xcodes, mas |
| `dot_cdk.json` | AWS CDK | CDK configuration |
| `dot_gitignore_global` (~2,800 bytes) | Git | Global ignore patterns |
| `dot_claude/` | Claude Code | Settings template and statusline script |

### Run Scripts (Installation & Configuration)

#### One-Time Scripts (`run_once_`)
| Script | Purpose | Platform Guards |
|--------|---------|-----------------|
| `run_once_install-tmux.sh.tmpl` | tmux + fzf installation | Darwin, Arch, Debian, RHEL |
| `run_once_install-java.sh.tmpl` | OpenJDK 21 for LTeX-LS grammar checking | Darwin, Arch, Debian, RHEL |
| `run_once_install-personal-packages.sh.tmpl` | Discord, NVIDIA Sync | Personal profile, not WSL/headless, Arch/macOS |
| `run_once_install-personal-cli-tools.sh.tmpl` | Claude Code, OpenCode | Personal profile, all platforms |
| `run_once_before_install-locale.sh.tmpl` | Locale setup | Linux only |
| `run_once_before_configure-cuda-dgx-spark.sh.tmpl` | CUDA 13.0+ config | DGX Spark only |
| `run_once_before_configure-framework-audio.sh.tmpl` | Framework laptop audio fixes | Framework laptops only |
| `run_once_before_setup-framework-fixes.sh.tmpl` | Framework lid switch, touchpad | Framework laptops only |

#### On-Change Scripts (`run_onchange_`)
| Script | Purpose | Size |
|--------|---------|------|
| `run_onchange_install-packages.sh.tmpl` | Core package installation | ~14KB, main installer |
| `run_onchange_before_configure-passwordless-sudo.sh.tmpl` | Passwordless sudo config | Small |
| `run_onchange_set-default-browser.sh.tmpl` | Default browser setup | Small |

#### Core Packages Installed (from `run_onchange_install-packages.sh.tmpl`)
**All Linux distros:** neovim, ripgrep, fzf, fd, jq, tree, cmake, go, nodejs, npm, python-pip/pipx, wget, direnv, shellcheck, pandoc-cli, tmux, zsh, p7zip, subversion, aria2, curl, stow, github-cli, nushell, starship, eza, typst, ttf-cascadia-mono-nerd, ttf-liberation

**Arch additions:** base-devel, bash-completion (via pacman)
**Debian additions:** build-essential, apt equivalents
**RHEL additions:** Development Tools group, dnf equivalents

### Utility Scripts (`bin/`)
| Script | Purpose |
|--------|---------|
| `sync_dotfiles` | Pull repo updates with stash/pop |
| `update_git` | Git update utility |
| `clean_git` | Git cleanup |
| `git_list_untracked` | List untracked files |
| `clean_zip` | ZIP cleanup |
| `get_region` | AWS region detection |
| `install_typst` | Typst installation |

### Local Binaries (`dot_local/bin/`)
| Script | Purpose |
|--------|---------|
| `executable_tmux-project-switch` | tmux project switching |
| `executable_tmux-session-new` | Create new tmux session |

## Platform Detection Details

### Detection Logic in `.chezmoi.toml.tmpl`

**Distribution detection uses multi-level fallback:**
1. Binary existence check (e.g., `/usr/bin/pacman` for Arch)
2. `osRelease.id` exact match (e.g., `"arch"`, `"debian"`)
3. `osRelease.idLike` contains match (e.g., idLike contains `"arch"`)

**Special system detection:**
- **WSL:** Checks for `"microsoft"` or `"wsl"` in kernel osrelease
- **Omarchy:** `isArchBased` AND Linux (Arch-based with Hyprland desktop)
- **DGX Spark:** Checks for `/etc/dgx-release` (ARM64 + Blackwell GPU)
- **Headless:** No DISPLAY, WAYLAND_DISPLAY, or MIR_SOCKET; override with `CHEZMOI_HEADLESS` env var
- **Framework laptops:** Detected by hostname pattern

**Profile detection priority:**
1. `CHEZMOI_PROFILE` environment variable (highest)
2. Hostname contains `"work"`, `"corp"`, or `"company"` -> `"work"`
3. Default: `"personal"`

### Complete Data Variables

```toml
[data]
    profile = "personal" | "work"
    email = "33106137+Kirizan@users.noreply.github.com"
    name = "Kirizan"
    isWSL = bool
    arch = "x86_64" | "arm64" | ...
    isArchBased = bool
    isDebianBased = bool
    isRHELBased = bool
    isOmarchy = bool
    useHomebrew = bool
    isHeadless = bool
    isDGXSpark = bool

[data.packages]
    manager = "brew" | "pacman" | "dnf" | "apt" | "winget"
    install_cmd = "brew install" | "sudo pacman -S" | ...
    update_cmd = "brew update && brew upgrade" | ...
    search_cmd = "brew search" | ...

[data.paths]
    homebrew = "/opt/homebrew"  # macOS only

[data.theme]
    name = "catppuccin-dark"  # Omarchy only
```

## `.chezmoiignore` Structure

Platform-conditional ignoring categories:
- **Always ignored:** README.md, LICENSE, Makefile, PROFILES.md, .gitignore, .git, templates/, CLAUDE.md, HOTKEYS.md, .org files, temp files
- **macOS ignores:** .bashrc, .bash_profile (Linux-only files)
- **Linux ignores:** .Brewfile, Library/, .zshrc, .zprofile (macOS-only files)
- **Tool-conditional:** Hyprland (if not installed), Waybar (if not installed), Mako (if not installed), Fish (if not installed), Omarchy (if ~/.local/share/omarchy missing)
- **Headless ignores:** alacritty, wezterm, autostart, gvimrc, browser setup, personal packages
- **Profile-conditional:** `*_work` files ignored on personal profile

## Git Workflow

- **Branches:** `chezmoi` (development), `main` (primary)
- **Remote:** `origin` with both branches
- Commit with descriptive messages
- NEVER push automatically - user controls when to push
