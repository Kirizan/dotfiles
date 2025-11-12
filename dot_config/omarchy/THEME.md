# Omarchy Theme Configuration

Current active theme: **osaka-jade**

## Setting the Theme

The Omarchy theme is managed by symlinks:
- Theme symlink: `~/.config/omarchy/current/theme -> ~/.local/share/omarchy/themes/osaka-jade`
- Background symlink: `~/.config/omarchy/current/background -> theme/backgrounds/...`

## Changing Theme

To change the Omarchy theme:

```bash
# List available themes
ls ~/.config/omarchy/themes/

# Change theme (replace 'osaka-jade' with desired theme)
ln -sfn ~/.local/share/omarchy/themes/osaka-jade ~/.config/omarchy/current/theme
```

## Available Themes

- osaka-jade (current)
- catppuccin
- catppuccin-latte
- everforest
- flexoki-light
- gruvbox
- kanagawa
- matte-black
- nord
- ristretto
- rose-pine
- tokyo-night

## Theme Integration

The active theme provides configurations for:
- Hyprland (window manager colors)
- Waybar (status bar)
- Mako (notifications)
- Kitty/Alacritty (terminals)
- Btop (system monitor)
- Neovim (editor colors)
- And more...
