set -euo pipefail

ARCH="$(uname -m)"
case "$ARCH" in
    x86_64|aarch64) ;;
    *) echo "Unsupported architecture: $ARCH"; exit 1 ;;
esac

# TODO: this repo has been archived and unmaintained. But this should be fine now
TAG="$(curl -fsSL "https://api.github.com/repos/blahgeek/emacs-appimage/releases" \
    | python3 -c "import json,sys; releases=json.load(sys.stdin); print(next(r['tag_name'] for r in releases if r['prerelease']))")"
APPIMAGE="Emacs-master-nox-${ARCH}.AppImage"
URL="https://github.com/blahgeek/emacs-appimage/releases/download/${TAG}/${APPIMAGE}"

mkdir -p "$HOME/.local/emacs"
curl -fsSL -o "$HOME/.local/emacs/emacs.AppImage" "$URL"
chmod +x "$HOME/.local/emacs/emacs.AppImage"

# symlink to emacsclient uses argv[0] trick: AppRun runs the binary matching basename $0
mkdir -p "$HOME/.local/bin"
ln -sf "$HOME/.local/emacs/emacs.AppImage" "$HOME/.local/bin/emacs"
ln -sf "$HOME/.local/emacs/emacs.AppImage" "$HOME/.local/bin/emacsclient"
