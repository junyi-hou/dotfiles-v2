#!/usr/bin/env bash
set -euo pipefail

command -v brew > /dev/null || {
    echo "'brew' not found, install it first."
    exit 1
}

brew install --cask nikitabobko/tap/aerospace
brew install jq

mkdir -p "$HOME/.local/bin"

echo ""
echo "AeroSpace installed."
echo "Next steps:"
echo "  1. Run 'make install' to symlink the AeroSpace config to ~/.config/aerospace/"
echo "  2. Start Emacs — i3-mode will symlink 'aerospace-call' to ~/.local/bin on first load"
echo "  3. Ensure ~/.local/bin is in your PATH"
