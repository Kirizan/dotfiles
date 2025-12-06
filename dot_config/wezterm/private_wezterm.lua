-- WezTerm Configuration
-- Cross-platform terminal emulator with GPU acceleration
-- https://wezfurlong.org/wezterm/

local wezterm = require 'wezterm'
local config = {}

-- Use config builder for better error messages (WezTerm 20220807-113146 and later)
if wezterm.config_builder then
  config = wezterm.config_builder()
end

-- Color scheme - Catppuccin Mocha (matches your catppuccin-dark theme)
config.color_scheme = 'Catppuccin Mocha'

-- Font configuration
config.font = wezterm.font_with_fallback {
  'CaskaydiaMono Nerd Font',
  'JetBrains Mono',
  'Noto Color Emoji',
}
config.font_size = 14.0

-- Window configuration
config.window_decorations = "RESIZE"
config.window_padding = {
  left = 8,
  right = 8,
  top = 8,
  bottom = 8,
}

-- Tab bar configuration
config.enable_tab_bar = true
config.use_fancy_tab_bar = true
config.hide_tab_bar_if_only_one_tab = false
config.tab_bar_at_bottom = false

-- Performance
config.front_end = "WebGpu"
config.max_fps = 120
config.animation_fps = 60

-- Scrollback
config.scrollback_lines = 10000

-- Cursor
config.cursor_blink_rate = 0  -- Disable cursor blinking
config.default_cursor_style = 'SteadyBlock'

-- Disable warning about missing glyphs
config.warn_about_missing_glyphs = false

-- Shell integration
config.set_environment_variables = {
  -- Use ISO 8601 timestamps
  LC_TIME = 'en_DK.UTF-8',
}

-- Default program (use login shell)
-- On macOS/Linux this will use your default shell (bash/zsh/nushell)
-- On Windows this will use PowerShell or your configured shell
if wezterm.target_triple == 'x86_64-pc-windows-msvc' then
  config.default_prog = { 'pwsh.exe', '-NoLogo' }
end

-- Key bindings
config.keys = {
  -- Tab navigation
  { key = 't', mods = 'CTRL|SHIFT', action = wezterm.action.SpawnTab 'CurrentPaneDomain' },
  { key = 'w', mods = 'CTRL|SHIFT', action = wezterm.action.CloseCurrentTab { confirm = true } },
  { key = 'Tab', mods = 'CTRL', action = wezterm.action.ActivateTabRelative(1) },
  { key = 'Tab', mods = 'CTRL|SHIFT', action = wezterm.action.ActivateTabRelative(-1) },

  -- Pane splitting
  { key = '|', mods = 'CTRL|SHIFT', action = wezterm.action.SplitHorizontal { domain = 'CurrentPaneDomain' } },
  { key = '_', mods = 'CTRL|SHIFT', action = wezterm.action.SplitVertical { domain = 'CurrentPaneDomain' } },

  -- Pane navigation
  { key = 'h', mods = 'CTRL|SHIFT', action = wezterm.action.ActivatePaneDirection 'Left' },
  { key = 'l', mods = 'CTRL|SHIFT', action = wezterm.action.ActivatePaneDirection 'Right' },
  { key = 'k', mods = 'CTRL|SHIFT', action = wezterm.action.ActivatePaneDirection 'Up' },
  { key = 'j', mods = 'CTRL|SHIFT', action = wezterm.action.ActivatePaneDirection 'Down' },

  -- Font size
  { key = '+', mods = 'CTRL', action = wezterm.action.IncreaseFontSize },
  { key = '-', mods = 'CTRL', action = wezterm.action.DecreaseFontSize },
  { key = '0', mods = 'CTRL', action = wezterm.action.ResetFontSize },

  -- Copy/Paste
  { key = 'c', mods = 'CTRL|SHIFT', action = wezterm.action.CopyTo 'Clipboard' },
  { key = 'v', mods = 'CTRL|SHIFT', action = wezterm.action.PasteFrom 'Clipboard' },
}

-- Mouse bindings
config.mouse_bindings = {
  -- Right click pastes from clipboard
  {
    event = { Down = { streak = 1, button = 'Right' } },
    mods = 'NONE',
    action = wezterm.action.PasteFrom 'Clipboard',
  },
}

-- Hyperlink rules (make URLs clickable)
config.hyperlink_rules = wezterm.default_hyperlink_rules()

-- Make username/project paths clickable
table.insert(config.hyperlink_rules, {
  regex = [[["]?([\w\d]{1}[-\w\d]+)(/){1}([-\w\d\.]+)["]?]],
  format = 'https://www.github.com/$1/$3',
})

return config
