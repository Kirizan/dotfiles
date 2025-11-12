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
NC := \033[0m # No Color

help: ## Show this help message
	@echo "$(CYAN)Dotfiles Management Makefile$(NC)"
	@echo ""
	@echo "Detected OS: $(GREEN)$(OS)$(NC)"
	@if [ "$(OS)" = "linux" ]; then \
		echo "Distribution: $(GREEN)$(DISTRO)$(NC)"; \
	fi
	@echo "Package Manager: $(GREEN)$(PKG_MANAGER)$(NC)"
	@echo ""
	@echo "$(YELLOW)Available targets:$(NC)"
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "  $(CYAN)%-20s$(NC) %s\n", $$1, $$2}'

setup: install-prereqs install-chezmoi apply ## Complete setup: install prerequisites, chezmoi, and apply dotfiles
	@echo "$(GREEN)✓ Setup complete!$(NC)"
	@echo ""
	@echo "Your dotfiles are now managed by chezmoi."
	@echo "Source directory: ~/.local/share/chezmoi"
	@echo ""
	@echo "Useful commands:"
	@echo "  make apply    - Apply all dotfiles"
	@echo "  make status   - Check status"
	@echo "  make update   - Update from git and apply"
	@echo "  make diff     - Show differences"

install-prereqs: ## Install system prerequisites
	@echo "$(CYAN)Installing prerequisites for $(OS)...$(NC)"
ifeq ($(OS),macos)
	@if ! command -v brew >/dev/null 2>&1; then \
		echo "$(YELLOW)Installing Homebrew...$(NC)"; \
		/bin/bash -c "$$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"; \
	else \
		echo "$(GREEN)✓ Homebrew already installed$(NC)"; \
	fi
else ifeq ($(DISTRO),arch)
	@echo "$(GREEN)✓ Using pacman (Arch Linux)$(NC)"
	@# Add any Arch-specific prerequisites here
else ifeq ($(DISTRO),debian)
	@echo "$(YELLOW)Updating package list...$(NC)"
	@sudo apt update
	@echo "$(GREEN)✓ Using apt (Debian/Ubuntu)$(NC)"
else
	@echo "$(YELLOW)⚠ Unknown distribution, skipping prerequisites$(NC)"
endif

install-chezmoi: ## Install chezmoi if not present
	@if command -v chezmoi >/dev/null 2>&1; then \
		echo "$(GREEN)✓ chezmoi already installed: $$(chezmoi --version | head -1)$(NC)"; \
	else \
		echo "$(CYAN)Installing chezmoi...$(NC)"; \
		if [ "$(OS)" = "macos" ]; then \
			brew install chezmoi; \
		elif [ "$(DISTRO)" = "arch" ]; then \
			sudo pacman -S --needed --noconfirm chezmoi; \
		elif [ "$(DISTRO)" = "debian" ]; then \
			sh -c "$$(curl -fsLS get.chezmoi.io)"; \
		else \
			sh -c "$$(curl -fsLS get.chezmoi.io)"; \
		fi; \
		echo "$(GREEN)✓ chezmoi installed$(NC)"; \
	fi

apply: check ## Apply dotfiles to system
	@echo "$(CYAN)Applying dotfiles...$(NC)"
	@chezmoi apply -v
	@echo "$(GREEN)✓ Dotfiles applied$(NC)"

init: ## Initialize chezmoi with this repository
	@echo "$(CYAN)Initializing chezmoi...$(NC)"
	@if [ ! -d ~/.local/share/chezmoi ]; then \
		chezmoi init --source=$$(pwd); \
		echo "$(GREEN)✓ Chezmoi initialized$(NC)"; \
	else \
		echo "$(YELLOW)⚠ Chezmoi already initialized$(NC)"; \
	fi

status: check ## Show chezmoi status
	@echo "$(CYAN)Chezmoi status:$(NC)"
	@chezmoi status || true

diff: check ## Show differences between dotfiles and system
	@echo "$(CYAN)Showing differences:$(NC)"
	@chezmoi diff || true

update: check ## Pull latest changes from git and apply
	@echo "$(CYAN)Updating dotfiles from git...$(NC)"
	@git pull
	@echo "$(CYAN)Applying changes...$(NC)"
	@chezmoi apply -v
	@echo "$(GREEN)✓ Updated and applied$(NC)"

verify: check ## Verify dotfiles without applying
	@echo "$(CYAN)Verifying dotfiles...$(NC)"
	@chezmoi verify
	@echo "$(GREEN)✓ Verification complete$(NC)"

doctor: check ## Run chezmoi doctor for diagnostics
	@echo "$(CYAN)Running chezmoi doctor...$(NC)"
	@chezmoi doctor

edit: check ## Open chezmoi source directory in editor
	@chezmoi cd

clean: ## Clean chezmoi cache
	@echo "$(CYAN)Cleaning chezmoi cache...$(NC)"
	@rm -rf ~/.cache/chezmoi
	@echo "$(GREEN)✓ Cache cleaned$(NC)"

check: ## Check if chezmoi is installed
	@if ! command -v chezmoi >/dev/null 2>&1; then \
		echo "$(RED)✗ chezmoi is not installed$(NC)"; \
		echo "Run: make install-chezmoi"; \
		exit 1; \
	fi

info: ## Show system and chezmoi information
	@echo "$(CYAN)System Information:$(NC)"
	@echo "  OS: $(OS)"
ifeq ($(OS),linux)
	@echo "  Distribution: $(DISTRO)"
endif
	@echo "  Package Manager: $(PKG_MANAGER)"
	@echo "  Shell: $$SHELL"
	@echo "  User: $$USER"
	@echo ""
	@if command -v chezmoi >/dev/null 2>&1; then \
		echo "$(CYAN)Chezmoi Information:$(NC)"; \
		chezmoi --version; \
		echo "  Source: $$(chezmoi source-path 2>/dev/null || echo 'Not initialized')"; \
		echo "  Config: ~/.config/chezmoi/chezmoi.toml"; \
	else \
		echo "$(YELLOW)⚠ chezmoi not installed$(NC)"; \
	fi
