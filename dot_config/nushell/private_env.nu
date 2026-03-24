# Nushell Environment Config File
# version = "0.108.0"

# Use ISO 8601 timestamps
$env.LC_TIME = "en_DK.UTF-8"

# Add custom bin directories to PATH
$env.PATH = ($env.PATH | split row (char esep) | append $"($env.HOME)/.dotfiles/bin")
$env.PATH = ($env.PATH | split row (char esep) | append $"($env.HOME)/.local/bin")

# Starship prompt initialization
mkdir ~/.cache/starship
starship init nu | save -f ~/.cache/starship/init.nu
