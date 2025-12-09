# Nushell Environment Config File
# version = "0.108.0"

# Use ISO 8601 timestamps
$env.LC_TIME = "en_DK.UTF-8"

# Initialize Homebrew on Linux
if ($nu.os-info.name == "linux") and ("/home/linuxbrew/.linuxbrew/bin/brew" | path exists) {
    $env.HOMEBREW_PREFIX = "/home/linuxbrew/.linuxbrew"
    $env.HOMEBREW_CELLAR = "/home/linuxbrew/.linuxbrew/Cellar"
    $env.HOMEBREW_REPOSITORY = "/home/linuxbrew/.linuxbrew/Homebrew"
    $env.PATH = ($env.PATH | split row (char esep) | prepend "/home/linuxbrew/.linuxbrew/bin")
    $env.PATH = ($env.PATH | split row (char esep) | prepend "/home/linuxbrew/.linuxbrew/sbin")
    $env.MANPATH = ($env.MANPATH? | default "" | split row (char esep) | prepend "/home/linuxbrew/.linuxbrew/share/man")
    $env.INFOPATH = ($env.INFOPATH? | default "" | split row (char esep) | prepend "/home/linuxbrew/.linuxbrew/share/info")
}

# Add custom bin directories to PATH
$env.PATH = ($env.PATH | split row (char esep) | append $"($env.HOME)/.dotfiles/bin")
$env.PATH = ($env.PATH | split row (char esep) | append $"($env.HOME)/.local/bin")

# Starship prompt initialization
mkdir ~/.cache/starship
starship init nu | save -f ~/.cache/starship/init.nu
