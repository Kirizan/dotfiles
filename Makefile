.PHONY: help setup install-chezmoi install-prereqs apply update status diff check clean

# Detect operating system
UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Darwin)
	OS := macos
	PKG_MANAGER := brew
else ifeq ($(UNAME_S),Linux)
	OS := linux
	# Detect Linux distribution
	ifeq ($(shell test -f /etc/arch-release && echo yes),yes)
		DISTRO := arch
		PKG_MANAGER := pacman
	else ifeq ($(shell test -f /etc/debian_version && echo yes),yes)
		DISTRO := debian
		PKG_MANAGER := apt
	else
		DISTRO := unknown
		PKG_MANAGER := unknown
	endif
else
	OS := unknown
	PKG_MANAGER := unknown
endif

# Colors for output
CYAN := \033[0;36m
GREEN := \033[0;32m
YELLOW := \033[0;33m
RED := \033[0;31m
NC := \033[0m

help: ## Show this help message
	@/bin/echo -e "$(CYAN)Dotfiles Management Makefile$(NC)\n"
	@/bin/echo -e "Detected OS: $(GREEN)$(OS)$(NC)"
	@if [ "$(OS)" = "linux" ]; then \
		/bin/echo -e "Distribution: $(GREEN)$(DISTRO)$(NC)"; \
	fi
	@/bin/echo -e "Package Manager: $(GREEN)$(PKG_MANAGER)$(NC)\n"
	@/bin/echo -e "$(YELLOW)Available targets:$(NC)"
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "  \033[0;36m%-20s\033[0m %s\n", $$1, $$2}'

setup: install-prereqs install-chezmoi apply ## Complete setup: install prerequisites, chezmoi, and apply dotfiles
	@/bin/echo -e "$(GREEN)✓ Setup complete!$(NC)\n"
	@/bin/echo -e "\n"
	@/bin/echo -e "Your dotfiles are now managed by chezmoi.\n"
	@/bin/echo -e "Source directory: ~/.local/share/chezmoi\n"
	@/bin/echo -e "\n"
	@/bin/echo -e "Useful commands:\n"
	@/bin/echo -e "  make apply    - Apply all dotfiles\n"
	@/bin/echo -e "  make status   - Check status\n"
	@/bin/echo -e "  make update   - Update from git and apply\n"
	@/bin/echo -e "  make diff     - Show differences\n"

install-prereqs: ## Install system prerequisites
	@/bin/echo -e "$(CYAN)Installing prerequisites for $(OS)...$(NC)\n"
ifeq ($(OS),macos)
	@if ! command -v brew >/dev/null 2>&1; then \
		/bin/echo -e "$(YELLOW)Installing Homebrew...$(NC)"; \
		/bin/bash -c "$$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"; \
	else \
		/bin/echo -e "$(GREEN)✓ Homebrew already installed$(NC)"; \
	fi
else ifeq ($(DISTRO),arch)
	@/bin/echo -e "$(GREEN)✓ Using pacman (Arch Linux)$(NC)\n"
	@# Add any Arch-specific prerequisites here
else ifeq ($(DISTRO),debian)
	@/bin/echo -e "$(YELLOW)Updating package list...$(NC)\n"
	@sudo apt update
	@/bin/echo -e "$(GREEN)✓ Using apt (Debian/Ubuntu)$(NC)\n"
else
	@/bin/echo -e "$(YELLOW)⚠ Unknown distribution, skipping prerequisites$(NC)\n"
endif

install-chezmoi: ## Install chezmoi if not present
	@if command -v chezmoi >/dev/null 2>&1; then \
		/bin/echo -e "$(GREEN)✓ chezmoi already installed: $$(chezmoi --version | head -1)$(NC)"; \
	else \
		/bin/echo -e "$(CYAN)Installing chezmoi...$(NC)"; \
		if [ "$(OS)" = "macos" ]; then \
			brew install chezmoi; \
		elif [ "$(DISTRO)" = "arch" ]; then \
			sudo pacman -S --needed --noconfirm chezmoi; \
		elif [ "$(DISTRO)" = "debian" ]; then \
			sh -c "$$(curl -fsLS get.chezmoi.io)"; \
		else \
			sh -c "$$(curl -fsLS get.chezmoi.io)"; \
		fi; \
		/bin/echo -e "$(GREEN)✓ chezmoi installed$(NC)"; \
	fi

apply: check ## Apply dotfiles to system
	@/bin/echo -e "$(CYAN)Applying dotfiles...$(NC)\n"
	@chezmoi apply -v
	@/bin/echo -e "$(GREEN)✓ Dotfiles applied$(NC)\n"

init: ## Initialize chezmoi with this repository
	@/bin/echo -e "$(CYAN)Initializing chezmoi...$(NC)\n"
	@if [ ! -d ~/.local/share/chezmoi ]; then \
		chezmoi init --source=$$(pwd); \
		/bin/echo -e "$(GREEN)✓ Chezmoi initialized$(NC)"; \
	else \
		/bin/echo -e "$(YELLOW)⚠ Chezmoi already initialized$(NC)"; \
	fi

status: check ## Show chezmoi status
	@/bin/echo -e "$(CYAN)Chezmoi status:$(NC)\n"
	@chezmoi status || true

diff: check ## Show differences between dotfiles and system
	@/bin/echo -e "$(CYAN)Showing differences:$(NC)\n"
	@chezmoi diff || true

update: check ## Pull latest changes from git and apply
	@/bin/echo -e "$(CYAN)Updating dotfiles from git...$(NC)\n"
	@git pull
	@/bin/echo -e "$(CYAN)Applying changes...$(NC)\n"
	@chezmoi apply -v
	@/bin/echo -e "$(GREEN)✓ Updated and applied$(NC)\n"

verify: check ## Verify dotfiles without applying
	@/bin/echo -e "$(CYAN)Verifying dotfiles...$(NC)\n"
	@chezmoi verify
	@/bin/echo -e "$(GREEN)✓ Verification complete$(NC)\n"

doctor: check ## Run chezmoi doctor for diagnostics
	@/bin/echo -e "$(CYAN)Running chezmoi doctor...$(NC)\n"
	@chezmoi doctor

edit: check ## Open chezmoi source directory in editor
	@chezmoi cd

clean: ## Clean chezmoi cache
	@/bin/echo -e "$(CYAN)Cleaning chezmoi cache...$(NC)\n"
	@rm -rf ~/.cache/chezmoi
	@/bin/echo -e "$(GREEN)✓ Cache cleaned$(NC)\n"

check: ## Check if chezmoi is installed
	@if ! command -v chezmoi >/dev/null 2>&1; then \
		/bin/echo -e "$(RED)✗ chezmoi is not installed$(NC)"; \
		/bin/echo -e "Run: make install-chezmoi"; \
		exit 1; \
	fi

info: ## Show system and chezmoi information
	@/bin/echo -e "$(CYAN)System Information:$(NC)\n"
	@/bin/echo -e "  OS: $(OS)\n"
ifeq ($(OS),linux)
	@/bin/echo -e "  Distribution: $(DISTRO)\n"
endif
	@/bin/echo -e "  Package Manager: $(PKG_MANAGER)\n"
	@/bin/echo -e "  Shell: $$SHELL\n"
	@/bin/echo -e "  User: $$USER\n"
	@/bin/echo -e "\n"
	@if command -v chezmoi >/dev/null 2>&1; then \
		/bin/echo -e "$(CYAN)Chezmoi Information:$(NC)"; \
		chezmoi --version; \
		/bin/echo -e "  Source: $$(chezmoi source-path 2>/dev/null || echo 'Not initialized')"; \
		/bin/echo -e "  Config: ~/.config/chezmoi/chezmoi.toml"; \
	else \
		/bin/echo -e "$(YELLOW)⚠ chezmoi not installed$(NC)"; \
	fi
