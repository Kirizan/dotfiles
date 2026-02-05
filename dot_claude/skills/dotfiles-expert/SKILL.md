---
name: dotfiles-expert
description: This skill should be used when the user asks to "add a package", "create a template", "modify shell config", "add a run script", "update chezmoi config", "fix dotfiles", "add platform support", or discusses chezmoi templates, platform detection, shell configuration, or cross-platform dotfile management in this repository.
---

# Dotfiles Repository Expert

This skill provides specialized knowledge of this chezmoi-managed dotfiles repository, covering its architecture, platform detection system, template conventions, and common modification patterns.

## Repository Overview

This repo is the chezmoi source directory for cross-platform dotfile management across macOS and Linux (Arch, Debian, RHEL families), with special handling for WSL, headless servers, Framework laptops, NVIDIA DGX Spark, and Omarchy desktop.

### Key Chezmoi Files

| File | Purpose |
|------|---------|
| `.chezmoi.toml.tmpl` | Master config: platform detection, data variables, package managers |
| `.chezmoiignore` | Platform-conditional file ignoring |
| `.chezmoiremove` | Files to remove from target |
| `.chezmoiexternal.toml` | External git repos (Omarchy Catppuccin theme) |

### Template Data Variables

Available in all `.tmpl` files (set by `.chezmoi.toml.tmpl`):

| Variable | Type | Description |
|----------|------|-------------|
| `.profile` | string | `"personal"` or `"work"` |
| `.isArchBased` | bool | Arch, CachyOS, EndeavourOS, Manjaro |
| `.isDebianBased` | bool | Debian, Ubuntu, Mint, Pop!_OS |
| `.isRHELBased` | bool | RHEL, CentOS, Fedora, Rocky, Alma |
| `.isWSL` | bool | Windows Subsystem for Linux |
| `.isHeadless` | bool | No GUI environment |
| `.isOmarchy` | bool | Arch-based with Omarchy desktop |
| `.isDGXSpark` | bool | NVIDIA DGX Spark system |
| `.useHomebrew` | bool | macOS with Homebrew |
| `.packages.manager` | string | `brew`, `pacman`, `dnf`, `apt`, or `winget` |
| `.packages.install_cmd` | string | Full install command |

### Standard Conditional Pattern

```go-template
{{- if eq .chezmoi.os "darwin" }}
# macOS
{{- else if eq .chezmoi.os "linux" }}
{{-   if .isArchBased }}
# Arch-based
{{-   else if .isDebianBased }}
# Debian-based
{{-   else if .isRHELBased }}
# RHEL-based
{{-   end }}
{{- end }}
```

## Common Tasks

### Adding a Package

- **All platforms:** Edit `run_onchange_install-packages.sh.tmpl` (add to each distro block)
- **macOS GUI apps:** Edit `dot_Brewfile.tmpl` (`cask "app"` or `brew "tool"`)
- **Personal-only:** Edit `run_once_install-personal-packages.sh.tmpl` (guarded by profile check)
- **Personal CLI tools:** Edit `run_once_install-personal-cli-tools.sh.tmpl`

### Creating a Template

Apply chezmoi naming: `dot_` prefix, `.tmpl` suffix, `private_` for 600, `executable_` for +x. Use the standard conditional pattern above with custom data variables for fine-grained control.

### Adding a Run Script

Follow naming conventions: `run_once_` (one-time), `run_onchange_` (re-run on change), `run_before_`/`run_after_` (ordering). Always use `.sh.tmpl` suffix, `#!/bin/bash` shebang, and ensure idempotency.

### Modifying Shell Config

- zsh (macOS): `dot_zshrc.tmpl`
- bash (Linux): `dot_bashrc.tmpl`
- fish (both): `dot_config/fish/config.fish.tmpl`
- Update multiple shell files when a feature should work across shells.

## Conventions

1. **Whitespace trimming:** Use `{{-` and `-}}` (established pattern)
2. **Nested indentation:** 2-space indent for nested template blocks: `{{-   if ... }}`
3. **Headless guards:** GUI additions use `{{ if not .isHeadless }}`
4. **Profile guards:** Personal-only features use `{{ if eq .profile "personal" }}`
5. **Testing:** Always recommend `chezmoi diff` and `chezmoi apply --dry-run --verbose`
6. **Git workflow:** Commit with descriptive messages, NEVER push automatically

## Additional Resources

### Reference Files

For detailed architecture, file listings, and task patterns, consult:
- **`references/architecture.md`** - Complete file inventory, platform detection details, shell config features, run script details, and application config breakdown
