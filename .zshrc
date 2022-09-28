######################################################################
####################     enable Powerlevel10k     ####################
######################################################################
# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

source /opt/homebrew/opt/powerlevel10k/powerlevel10k.zsh-theme

# To customize prompt, run `p10k configure` or edit ~/.config/powerlevel10k/p10k.zsh.
[[ ! -f ~/.config/powerlevel10k/p10k.zsh ]] || source ~/.config/powerlevel10k/p10k.zsh


####################################################################
####################     Path configuration     ####################
####################################################################
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
export PATH="$PATH:/Users/nikirby/.dotfiles/bin"


#########################################################
####################     Aliases     ####################
#########################################################

alias lsa='gls -liahF --group-directories-first --color=auto'
alias c='clear; ls -G'

# register all sub accounts into Isengard
update_isengard() {
  isengardcli register-org-accounts $1 --role AWSControlTowerExecution --secondary-owner bmperk
}

# register all Isengard accounts with Account Guardian 
alias account_guarding_enroll='npx ts-node /Users/nikirby/Applications/account-guardian-onboarding/index.ts'

# Force the CDK to use the AWS_PROFILE variable
alias cdkp='cdk --profile $AWS_PROFILE'

# Quick copies AWS Copyright onto the clipboard
alias copyright='pbcopy < /Users/nikirby/source_code/copyright_aws.txt'

# Changes terraform to tf
alias tf='terraform'

# Adds a Terraform auto approve alias
alias tfaa='tf apply -auto-approve'

# Adds a zip cleaning function
zip-clean() {
  zip -d "$1" "*/.idea/*" "__MACOSX/*" "*/*.DS_Store"
}

# Create template Terraform Module
tf_module_new() {
  mkdir $1
  touch $1/main.tf
  touch $1/variables.tf
  touch $1/output.tf
}

# using neovim instead of vim
alias vim="nvim"
alias vi="nvim"

# setup git defender
alias setup_defender="git defender --setup"

## AWS cli shortcuts

# Get current region
alias get_region="aws ec2 describe-availability-zones --output text --query 'AvailabilityZones[0].[RegionName]'"


##################################################################
####################     Bash Completions     ####################
##################################################################

##add completion directory to fpath
fpath=(~/.config/zsh/completion $fpath)

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


#move the completion file to ~/.config/zsh directory
compinit -d ~/.config/zsh/zcompdump
#### enable AWS cli completion
complete -C aws_completer aws

#### configure zsh completion
setopt noautomenu
setopt nomenucomplete

# partial completion suggestions
zstyle ':completion:*' list-suffixes zstyle ':completion:*' expand prefix suffix 

# add fzf auto-completion
[[ $- == *i* ]] && source "/opt/homebrew/opt/fzf/shell/completion.bash" 2> /dev/null


##########################################################################
####################     Move Configuration Files     ####################
##########################################################################

# moves the vim config file
export MYVIMRC='~/.config/vim/vimrc'
export VIMINIT='source $MYVIMRC'

# moves the AWS files
# Commented out while determining how to get this to be applied in PyCharm
# export AWS_CONFIG_FILE='~/.config/aws/config'
# export AWS_SHARED_CREDENTIALS_FILE='~/.config/aws/credentials'

# move the Isengard CLI config file
export ISENGARDCLI_CONFIG_FILE='~/.config/isengardcli/isengard_config'

# move global git-config file
export GIT_CONFIG_GLOBAL='/Users/nikirby/.config/git/config'


###############################################################
####################     Eval commands     ####################
###############################################################


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

######################## enable isengardcli shell integration #########
if command -v isengardcli 1>/dev/null 2>&1; then
  eval "$(isengardcli shell-profile)"
fi

# Add homebrew
eval "$(/opt/homebrew/bin/brew shellenv)"


#######################################################################
####################     Configure ZSH History     ####################
#######################################################################


######################### history options ############################
setopt EXTENDED_HISTORY        # store time in history
setopt HIST_EXPIRE_DUPS_FIRST  # unique events are more usefull to me
setopt HIST_VERIFY             # Make those history commands nice
setopt HIST_IGNORE_SPACE       # Make history ignore commands that start with a space. Good for exporting passwords without storing them to history.
setopt HIST_IGNORE_ALL_DUPS    # History ignores duplicate commands
setopt INC_APPEND_HISTORY      # immediatly insert history into history file
HISTSIZE=1000000               # Sets history size to 1,000,000 lines
SAVEHIST=$HISTSIZE             # Sets save history size to equal to history size

#######################################################################
####################     Configure ZSH Options     ####################
#######################################################################

setopt NO_BEEP                 # Stops terminal beeps

# configures local to ensure proper sorting
# default is LC_COLLATE="en_US.UTF-8"
export LC_COLLATE="cs_CZ.ISO8859-2"


# fzf key bindings
source "/opt/homebrew/opt/fzf/shell/key-bindings.zsh"

###################################################################
####################     Configure GO Lang     ####################
###################################################################

# Stops go lang from using Google managed proxies

go env -w GOPROXY=direct


###########################################################################
####################     Additional Configuration'     ####################
###########################################################################

# This section is for configurations that shouldn't get synced to github
# Mainly used for configurations unique to my work environment

if test -f "~/.dotfile/local/zsh/zshrc"; then
  source "~/.dotfile/local/zsh/zshrc"
fi

