# Nushell Config File
# version = "0.108.0"

# Load Starship prompt
use ~/.cache/starship/init.nu

# Common aliases
alias tf = terraform
alias tfaa = terraform apply -auto-approve
alias vim = nvim
alias vi = nvim
alias cdkp = cdk --profile $env.AWS_PROFILE

# ls with colors (using eza if available)
def lsa [] {
  if (which eza | is-not-empty) {
    eza -lahF --group-directories-first --git --icons
  } else {
    ls -la
  }
}

# Configure completion
$env.config = {
  show_banner: false
  completions: {
    case_sensitive: false
    quick: true
    partial: true
  }
  history: {
    max_size: 1000000
    sync_on_enter: true
    file_format: "plaintext"
  }
}
