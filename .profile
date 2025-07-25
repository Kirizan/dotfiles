# configures curl-openssl path options.
export PATH="/usr/local/opt/curl-openssl/bin:$PATH"

# add brew's sbin to path
export PATH="/usr/local/sbin:$PATH"

# add homebrew's bin to path
if [ -d "/opt/homebrew" ]; then
  export PATH="/opt/homebrew/bin:$PATH"
fi

# add fzf to path
if [[ ! "$PATH" == */opt/homebrew/opt/fzf/bin* ]]; then
  PATH="${PATH:+${PATH}:}/opt/homebrew/opt/fzf/bin"
fi

# Added by JetBrains Toolbox App
export PATH="$PATH:/Users/nikirby/Library/Application Support/JetBrains/Toolbox/scripts"

# Add custom bin directory
export PATH="$PATH:$HOME/.dotfiles/bin"

# Add pipx bin to PATH

if [ -d "$HOME/.local/bin" ]; then
  export PATH="$PATH:$HOME/.local/bin"
fi

#Check if local/bin exists and add to path if it does
if [ -d "$HOME/.dotfiles/local/bin" ]; then
    export PATH="$PATH:$HOME/.dotfiles/local/bin"
fi

# Add doom emacs to path
if [ -d "$HOME/.config/emacs-doom/bin" ]; then
  export PATH="$PATH:$HOME/.config/emacs-doom/bin"
fi

# Add gnu versions of tools to path
if [ -d "/opt/homebrew/opt/grep/libexec/gnubin" ]; then
  PATH="/opt/homebrew/opt/grep/libexec/gnubin:$PATH"
fi

#Create alias for ls
alias lsa='gls -liahF --group-directories-first --color=auto --time-style=iso'

# Creates alias to launch Spacemacs config
alias spacemacs='emacs --with-profile spacemacs &'

# Changes terraform to tf
alias tf='terraform'

# Adds a Terraform auto approve alias
alias tfaa='tf apply -auto-approve'

# using neovim instead of vim
alias vim="nvim"
alias vi="nvim"

# Force the CDK to use the AWS_PROFILE variable
alias cdkp='cdk --profile $AWS_PROFILE'

# Create alias for pipx local run
alias pipxr='pipx run --spec .'

# Alias to activate poetry environment
alias activate_poetry='eval $(poetry env activate)'

# Alias for terraform
alias tf='terraform'

#move the completion file to ~/.config/zsh directory
autoload -Uz compinit
compinit -d ~/.config/zsh/zcompdump

#add completion directory to fpath
fpath=(~/.config/zsh/completion $fpath)

# configure zsh completion
setopt noautomenu
setopt nomenucomplete

# partial completion suggestions
zstyle ':completion:*' list-suffixes zstyle ':completion:*' expand prefix suffix 

# load bash completion scripts
autoload -U +X bashcompinit && bashcompinit
autoload -U +X compinit && compinit

if type brew &>/dev/null
then
  FPATH="$(brew --prefix)/share/zsh/site-functions:${FPATH}"
  FPATH="$(brew --prefix)/share/zsh-completions:$FPATH"

  autoload -Uz compinit
  compinit
fi

# pipx autocompletion
eval "$(register-python-argcomplete pipx)"

# enable AWS cli completion
complete -C aws_completer aws

# add fzf auto-completion
[[ $- == *i* ]] && source "/opt/homebrew/opt/fzf/shell/completion.bash" 2> /dev/null

######################## enable pyenv shims and autocomplete #########
if command -v pyenv 1>/dev/null 2>&1; then
  eval "$(pyenv init --path)"
  export PYENV_ROOT="$HOME/.pyenv"
  export PATH="$PYENV_ROOT/bin:$PATH"
  eval "$(pyenv init -)"
fi

######################## enable direnv ################################
if command -v direnv 1>/dev/null 2>&1; then
  eval "$(direnv hook zsh)"
fi

# Add homebrew
eval "$(/opt/homebrew/bin/brew shellenv)"

######################### history options ############################
setopt EXTENDED_HISTORY        # store time in history
setopt HIST_EXPIRE_DUPS_FIRST  # unique events are more usefull to me
setopt HIST_VERIFY             # Make those history commands nice
setopt HIST_IGNORE_SPACE       # Make history ignore commands that start with a space. Good for exporting passwords without storing them to history.
setopt HIST_IGNORE_ALL_DUPS    # History ignores duplicate commands
setopt INC_APPEND_HISTORY      # immediatly insert history into history file
HISTSIZE=1000000               # Sets history size to 1,000,000 lines
SAVEHIST=$HISTSIZE             # Sets save history size to equal to history size

# Disable terminal beebs
setopt NO_BEEP
# configures local to ensure proper sorting
# default is LC_COLLATE="en_US.UTF-8"
export LC_COLLATE="cs_CZ.ISO8859-2"

# fzf key bindings
source "/opt/homebrew/opt/fzf/shell/key-bindings.zsh"

# Stops go lang from using Google managed proxies
go env -w GOPROXY=direct

# This section is for configurations that shouldn't get synced to github
# Mainly used for configurations unique to my work environment

if test -f "/Users/nikirby/.config/zsh/zshrc"; then
  source "/Users/nikirby/.config/zsh/zshrc"
fi
