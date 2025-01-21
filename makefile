default:
	echo "Run make <command> to perform configurations"

install_brew:
ifneq (,$(wildcard $("/opt/homebrew")))
	/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
else
	echo "Brew already installed"
endif

brew_packages_install:
	brew bundle install --file=./Brewfile

brew_packages_update: Brewfile
	brew bundle dump --file=./Brewfile --force

install_fonts:
	brew search '/font-.*-nerd-font/' | awk '{ print $1 }' | xargs brew install --cask
	brew install --cask font-source-code-pro

install_doom_emacs:
	git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.config/emacs-doom

install_spacemacs:
	git clone --depth 1 https://github.com/syl20bnr/spacemacs ~/.config/emacs-spacemacs

install chem2:
	git clone --depth 1 https://github.com/plexus/chemacs2.git ~/.config/emacs

stow:
	stow .

default_emacs:
	set_emacs_default

set_global_git_ignore: 
	git config --global core.excludesFile "~/.gitignore_global"

setup_new: install_brew brew_packages_install install_doom_emacs stow install_fonts set_emacs_default



