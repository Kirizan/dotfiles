# Chezmoi Profiles

This dotfiles repository supports **work** and **personal** profiles, allowing you to maintain different configurations on different machines while sharing common configs.

## How It Works

### Profile Detection

The profile is automatically detected based on:

1. **Environment Variable** (highest priority):
   ```bash
   export CHEZMOI_PROFILE=work
   chezmoi apply
   ```

2. **Hostname Pattern** (automatic):
   - Hostname contains "work", "corp", or "company" → **work** profile
   - Otherwise → **personal** profile (default)

### Profile-Specific Settings

The active profile determines:

- **Git user name and email** (`.gitconfig`)
- **Which files are deployed** (via `.chezmoiignore`)
- **Profile-specific configs** (via template conditionals)

## Using Profiles

### Current Profile

Check your current profile:
```bash
chezmoi data | grep profile
```

On your current system (hostname: `kirdoit`):
- **Profile**: personal (default)
- **Email**: 33106137+Kirizan@users.noreply.github.com
- **Name**: Kirizan

### Setting Up a Work Machine

**Option 1: Automatic (via hostname)**
- Name your work machine with "work", "corp", or "company" in the hostname
- Example: `work-laptop`, `corp-desktop`

**Option 2: Manual (via environment variable)**
```bash
# In your shell rc file on work machine:
export CHEZMOI_PROFILE=work

# Then apply:
chezmoi apply
```

**Option 3: Edit hostname patterns**
Edit `.chezmoi.toml.tmpl` to add custom hostname patterns:
```toml
{{- else if contains "mycompany" .chezmoi.hostname -}}
{{-   $profile = "work" -}}
```

### Customizing Profile Settings

Edit `.chezmoi.toml.tmpl` to customize work profile settings:

```toml
{{- if eq $profile "work" -}}
{{-   $email = "your.name@company.com" -}}
{{-   $name = "Your Full Name" -}}
{{- end -}}
```

## Profile-Specific Files

### File Naming Convention

Files with these suffixes are profile-specific:

- **`*_work`** or **`*_work.*`** → Only deployed on **work** profile
- **`*_personal`** or **`*_personal.*`** → Only deployed on **personal** profile

### Examples

**Personal-only aliases:**
```bash
# dot_profile_personal
alias personal-server='ssh my-home-server'
alias personal-backup='rsync -av ~/docs user@home.local:/backup'
```

**Work-only configs:**
```bash
# dot_profile_work
export COMPANY_PROXY=http://proxy.company.com:8080
alias vpn='sudo openconnect vpn.company.com'
```

**In templates:**
```bash
# dot_bashrc.tmpl
source ~/.profile

{{- if eq .profile "personal" }}
if [ -f ~/.profile_personal ]; then
  source ~/.profile_personal
fi
{{- end }}

{{- if eq .profile "work" }}
if [ -f ~/.profile_work ]; then
  source ~/.profile_work
fi
{{- end }}
```

### Profile-Specific Directories

Create these directories for profile-specific configs:

- `~/.config/personal/` → Personal-only configs
- `~/.config/work/` → Work-only configs

Example:
```bash
# Create work-specific SSH config
chezmoi add ~/.config/work/ssh_config
```

## Git Configuration

The `.gitconfig` uses profiles automatically:

**Personal profile:**
- Uses personal email/name globally
- Includes `~/.gitconfig_personal` for personal repos

**Work profile:**
- Uses work email/name globally
- Includes `~/.gitconfig_corp` for work repos
- May include company-specific SSL certs

## Common Scenarios

### Scenario 1: New Personal Machine
```bash
chezmoi init https://github.com/yourusername/dotfiles.git
chezmoi apply
# Profile: personal (automatic)
```

### Scenario 2: New Work Machine
```bash
# Option A: Set hostname (permanent)
sudo hostnamectl set-hostname work-laptop

# Option B: Set environment variable
export CHEZMOI_PROFILE=work
echo 'export CHEZMOI_PROFILE=work' >> ~/.bash_profile

# Apply dotfiles
chezmoi init https://github.com/yourusername/dotfiles.git
chezmoi apply
# Profile: work
```

### Scenario 3: Testing Work Profile on Personal Machine
```bash
CHEZMOI_PROFILE=work chezmoi diff
# See what would change with work profile
```

### Scenario 4: Adding Work-Specific Config
```bash
# Create the file
vim ~/.aws/config_work

# Add to chezmoi with work suffix
chezmoi add --template ~/.aws/config_work

# Or create it directly in dotfiles
vim ~/.local/share/chezmoi/dot_aws/config_work.tmpl
```

## Verification

Check which files are managed on current profile:
```bash
chezmoi managed
```

Compare personal vs work profiles:
```bash
# Current (personal)
chezmoi managed

# Simulate work profile
CHEZMOI_PROFILE=work chezmoi managed
```

## Tips

1. **Keep secrets separate**: Use `private_` prefix for sensitive work files
2. **Test before applying**: Use `chezmoi diff` or `chezmoi apply --dry-run`
3. **Version control**: Both profiles are in the same repo, different files are just ignored
4. **Cross-platform**: Profiles work on any OS (macOS, Linux, etc.)
