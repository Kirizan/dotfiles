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

install_doom_emacs:
	git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.config/emacs

stow:
	stow .

install_fonts:
	brew search '/font-.*-nerd-font/' | awk '{ print $1 }' | xargs brew install --cask

setup_new: install_brew brew_packages_install install_doom_emacs stow install_fonts



