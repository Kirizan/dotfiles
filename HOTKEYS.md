# Hotkey Reference Guide

Quick reference for all keyboard shortcuts across Neovim, Tmux, and terminal tools.

---

## Neovim - Leader Key Mappings

**Leader key:** `<Space>`

### Code Quality & LSP (`<leader>kc`)
| Hotkey | Action |
|--------|--------|
| `<leader>kca` | Show code actions / fixes |
| `<leader>kcl` | List all issues (diagnostics) |
| `<leader>kcn` | Jump to next issue |
| `<leader>kcp` | Jump to previous issue |
| `<leader>kch` | Show issue details (hover) |
| `<leader>kci` | Show diagnostic info |
| `<leader>kcr` | Refresh checks (save file) |
| `<leader>kcR` | Restart LSP server |
| `<leader>kct` | Toggle live checking / check-on-save |

### Grammar (`<leader>kG`)
| Hotkey | Action |
|--------|--------|
| `<leader>kG` | Grammarly actions |

### Git (`<leader>kg`)
| Hotkey | Action |
|--------|--------|
| `<leader>kg` | Open Neogit (Git UI) |

### Terminal (`<leader>kt`)
| Hotkey | Action |
|--------|--------|
| `<leader>kt` | Toggle terminal |

### Typst (`<leader>kT`)
| Hotkey | Action |
|--------|--------|
| `<leader>kTa` | Add acronym |
| `<leader>kTi` | Insert acronym reference |
| `<leader>kTc` | Cycle acronym format |
| `<leader>kTb` | Add bibliography entry |
| `<leader>kTr` | Insert citation |

### Projects & Tmux (`<leader>kp`)
| Hotkey | Action |
|--------|--------|
| `<leader>kpn` | New project from template |
| `<leader>kpp` | Switch project (Telescope) |
| `<leader>kpf` | Find files in project |
| `<leader>kpg` | Grep in project |
| `<leader>kpx` | Tmux SessionX (fuzzy finder) |
| `<leader>kps` | Switch tmux session |
| `<leader>kpd` | Detach from tmux session |
| `<leader>kpk` | Kill tmux session |
| `<leader>kpl` | List tmux sessions |
| `<leader>kpr` | Rename tmux session |

### Project-Specific Hotkeys (`<leader>p`)
*Available when in specific project types*

**Python Projects:**
| Hotkey | Action |
|--------|--------|
| `<leader>pr` | Run main.py with Poetry |
| `<leader>pt` | Run tests with pytest |
| `<leader>pf` | Format code with black |

**TypeScript/Node Projects:**
| Hotkey | Action |
|--------|--------|
| `<leader>pr` | Run dev server (npm run dev) |
| `<leader>pb` | Build project (npm run build) |

**Typst Projects:**
| Hotkey | Action |
|--------|--------|
| `<leader>pb` | Build PDF (typst compile) |
| `<leader>pw` | Watch mode (auto-rebuild) |

---

## Tmux Hotkeys

**Prefix key:** `Ctrl+a` (instead of default `Ctrl+b`)

### Session Management
| Hotkey | Action |
|--------|--------|
| `Prefix + o` | Open SessionX (fuzzy session finder) |
| `Alt+p` | Quick project switch (global, no prefix) |
| `Prefix + d` | Detach from session |
| `Prefix + $` | Rename session |
| `Prefix + s` | List sessions |

### Window Management
| Hotkey | Action |
|--------|--------|
| `Prefix + c` | Create new window |
| `Prefix + ,` | Rename window |
| `Prefix + &` | Kill window |
| `Prefix + n` | Next window |
| `Prefix + p` | Previous window |
| `Prefix + 0-9` | Switch to window N |

### Pane Management
| Hotkey | Action |
|--------|--------|
| `Prefix + \|` | Split pane horizontally |
| `Prefix + -` | Split pane vertically |
| `Alt+←/→/↑/↓` | Switch panes (no prefix needed) |
| `Prefix + h/j/k/l` | Resize pane left/down/up/right |
| `Prefix + z` | Toggle pane zoom (fullscreen) |
| `Prefix + x` | Kill pane |
| `Prefix + q` | Show pane numbers |

### Copy Mode (Vi-style)
| Hotkey | Action |
|--------|--------|
| `Prefix + [` | Enter copy mode |
| `v` | Begin selection (in copy mode) |
| `y` | Copy selection and exit |
| `Prefix + ]` | Paste buffer |
| `q` | Exit copy mode |

### System
| Hotkey | Action |
|--------|--------|
| `Prefix + r` | Reload tmux configuration |
| `Prefix + ?` | Show all keybindings |
| `Prefix + t` | Show time |

---

## Terminal Hotkeys

### Shell (Bash/Zsh)
| Hotkey | Action |
|--------|--------|
| `Ctrl+r` | Reverse search history (fzf) |
| `Ctrl+t` | Fuzzy file search (fzf) |
| `Alt+c` | Fuzzy directory change (fzf) |
| `Ctrl+l` | Clear screen |
| `Ctrl+c` | Cancel current command |
| `Ctrl+d` | Exit shell / EOF |
| `Ctrl+z` | Suspend process (use `fg` to resume) |

### WezTerm Terminal
| Hotkey | Action |
|--------|--------|
| `Ctrl+Shift+t` | New tab |
| `Ctrl+Shift+w` | Close tab |
| `Ctrl+Tab` | Next tab |
| `Ctrl+Shift+Tab` | Previous tab |
| `Ctrl+Shift+\|` | Split horizontal |
| `Ctrl+Shift+_` | Split vertical |
| `Ctrl+Shift+h/j/k/l` | Navigate panes |
| `Ctrl++` | Increase font size |
| `Ctrl+-` | Decrease font size |
| `Ctrl+0` | Reset font size |
| `Ctrl+Shift+c` | Copy |
| `Ctrl+Shift+v` | Paste |

---

## Custom Scripts

### Tmux Project Scripts
| Command | Description |
|---------|-------------|
| `tmux-project-switch` | Fuzzy search projects and tmux sessions |
| `tmux-session-new PROJECT [PATH]` | Create new tmux session with layout |

---

## Tips

**Tmux Session Workflow:**
1. `Alt+p` - Quick switch between projects/sessions (anywhere)
2. `Prefix + o` - Open SessionX for advanced session search
3. Sessions persist across reboots (tmux-resurrect + tmux-continuum)

**Neovim Project Workflow:**
1. `<leader>kpn` - Create new project from template
2. `<leader>kpp` - Switch to existing project
3. `<leader>kpf/g` - Search files/text in project
4. Projects auto-create tmux sessions

**Copy/Paste:**
- Tmux: `Prefix+[` → `v` (select) → `y` (copy) → `Prefix+]` (paste)
- WezTerm: `Ctrl+Shift+c/v`
- System clipboard integration enabled on all platforms
