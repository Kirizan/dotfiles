#!/bin/bash
# Un-stow dotfiles on macOS before migrating to chezmoi
#
# Usage: ./unstow_mac.sh
#
# This script will remove all symlinks created by GNU Stow from your home directory.
# Run this before migrating to chezmoi to avoid conflicts.

set -e

# Color codes for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

echo -e "${CYAN}=====================================${NC}"
echo -e "${CYAN}  Dotfiles Un-stow Script (macOS)  ${NC}"
echo -e "${CYAN}=====================================${NC}\n"

# Check if running on macOS
if [[ "$OSTYPE" != "darwin"* ]]; then
    echo -e "${RED}Error: This script is intended for macOS only.${NC}"
    echo -e "${YELLOW}Current OS: $OSTYPE${NC}"
    exit 1
fi

# Check if stow is installed
if ! command -v stow >/dev/null 2>&1; then
    echo -e "${RED}Error: GNU Stow is not installed.${NC}"
    echo -e "${YELLOW}If you didn't use Stow, you may not need this script.${NC}"
    exit 1
fi

# Get the directory where this script is located
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

echo -e "${CYAN}Dotfiles directory:${NC} $SCRIPT_DIR\n"

# Confirm with user
echo -e "${YELLOW}⚠ This will remove all symlinks created by Stow from your home directory.${NC}"
echo -e "${YELLOW}⚠ Make sure you have backups of any important configurations!${NC}\n"
read -p "Do you want to continue? (y/N): " -n 1 -r
echo
if [[ ! $REPLY =~ ^[Yy]$ ]]; then
    echo -e "\n${YELLOW}Un-stow cancelled.${NC}"
    exit 0
fi

echo -e "\n${CYAN}Un-stowing dotfiles...${NC}\n"

# Change to dotfiles directory
cd "$SCRIPT_DIR"

# Run stow with delete flag and verbose output
if stow -D -v . 2>&1 | tee /tmp/unstow_output.log; then
    echo -e "\n${GREEN}✓ Successfully un-stowed dotfiles!${NC}\n"

    # Show summary
    echo -e "${CYAN}Summary:${NC}"
    echo -e "  - Removed symlinks from: ${HOME}"
    echo -e "  - Source directory: ${SCRIPT_DIR}"
    echo -e "  - Full log: /tmp/unstow_output.log\n"

    echo -e "${GREEN}You can now proceed with chezmoi migration.${NC}"
    echo -e "${YELLOW}Recommended next steps:${NC}"
    echo -e "  1. Switch to chezmoi branch: ${CYAN}git checkout chezmoi${NC}"
    echo -e "  2. Initialize chezmoi: ${CYAN}cd ~/.local/share/chezmoi && chezmoi init${NC}"
    echo -e "  3. Apply dotfiles: ${CYAN}chezmoi apply${NC}\n"
else
    echo -e "\n${RED}✗ Error during un-stow operation.${NC}"
    echo -e "${YELLOW}Check /tmp/unstow_output.log for details.${NC}\n"
    exit 1
fi

# Optional: List remaining symlinks in home directory (for verification)
echo -e "${CYAN}Checking for remaining symlinks in home directory...${NC}"
remaining=$(find "$HOME" -maxdepth 1 -type l 2>/dev/null | wc -l)
if [ "$remaining" -gt 0 ]; then
    echo -e "${YELLOW}Found $remaining symlinks in home directory:${NC}"
    find "$HOME" -maxdepth 1 -type l -ls 2>/dev/null | head -10
    if [ "$remaining" -gt 10 ]; then
        echo -e "${YELLOW}... and $((remaining - 10)) more${NC}"
    fi
    echo -e "\n${YELLOW}Note: Some of these may be created by other tools (not Stow).${NC}"
else
    echo -e "${GREEN}No symlinks found in home directory.${NC}"
fi

echo -e "\n${GREEN}Done!${NC}\n"
