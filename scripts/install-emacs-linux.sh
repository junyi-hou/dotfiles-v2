set -euo pipefail

ARCH="$(uname -m)"
case "$ARCH" in
    x86_64|aarch64) ;;
    *) echo "Unsupported architecture: $ARCH"; exit 1 ;;
esac

# TODO: this repo has been archived and unmaintained. But this should be fine now
VERSION="30.2"
APPIMAGE="Emacs-${VERSION}-nox-${ARCH}.AppImage"
URL="https://github.com/blahgeek/emacs-appimage/releases/download/${VERSION}/${APPIMAGE}"

mkdir -p "$HOME/.local/bin"
curl -fsSL -o "$HOME/.local/bin/emacs" "$URL"
chmod +x "$HOME/.local/bin/emacs"
