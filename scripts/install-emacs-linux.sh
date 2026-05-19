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

TMPDIR="$(mktemp -d)"
trap 'rm -rf "$TMPDIR"' EXIT

curl -fsSL -o "$TMPDIR/$APPIMAGE" "$URL"
chmod +x "$TMPDIR/$APPIMAGE"

# --appimage-extract writes squashfs-root into $PWD
(cd "$TMPDIR" && "./$APPIMAGE" --appimage-extract)

rm -rf "$HOME/.local/emacs"
mv "$TMPDIR/squashfs-root" "$HOME/.local/emacs"

mkdir -p "$HOME/.local/bin"
ln -sf "$HOME/.local/emacs/usr/bin/emacs" "$HOME/.local/bin/emacs"
ln -sf "$HOME/.local/emacs/usr/bin/emacsclient" "$HOME/.local/bin/emacsclient"
