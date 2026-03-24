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
