set -euo pipefail

ARCH="$(uname -m)"
case "$ARCH" in
    x86_64|aarch64) ;;
    *) echo "Unsupported architecture: $ARCH"; exit 1 ;;
esac

# TODO: this repo has been archived and unmaintained. But this should be fine now
APPIMAGE="Emacs-30.2-nox-${ARCH}.AppImage"
URL="https://github.com/blahgeek/emacs-appimage/releases/download/github-action-build-17008212441/${APPIMAGE}"

mkdir -p "$HOME/.local/emacs"
curl -fsSL -o "$HOME/.local/emacs/emacs.AppImage" "$URL"
chmod +x "$HOME/.local/emacs/emacs.AppImage"

# Extract for emacsclient: AppRun sets up LD_LIBRARY_PATH from the extracted tree
TMPDIR="$(mktemp -d)"
trap 'rm -rf "$TMPDIR"' EXIT
(cd "$TMPDIR" && "$HOME/.local/emacs/emacs.AppImage" --appimage-extract)
rm -rf "$HOME/.local/emacs/squashfs-root"
mv "$TMPDIR/squashfs-root" "$HOME/.local/emacs/squashfs-root"

mkdir -p "$HOME/.local/bin"
ln -sf "$HOME/.local/emacs/emacs.AppImage" "$HOME/.local/bin/emacs"
# Use extracted AppRun for emacsclient so it gets the right LD_LIBRARY_PATH
cat > "$HOME/.local/bin/emacsclient" << 'EOF'
#!/bin/sh
$HOME/.local/emacs/squashfs-root/bin/emacsclient "$@"
EOF
chmod +x "$HOME/.local/bin/emacsclient"
