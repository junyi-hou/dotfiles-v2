#!/usr/bin/env bash
set -euo pipefail

EMACS_VERSION="31.0.90"
EMACS_SRC="https://alpha.gnu.org/gnu/emacs/pretest/emacs-${EMACS_VERSION}.tar.xz"
INSTALL_DIR="$HOME/.local/emacs"
BIN_DIR="$HOME/.local/bin"

# Try common package managers to install build-time dependencies (system path, non-persistent)
install_build_deps() {
    if command -v apt-get >/dev/null 2>&1; then
        echo "Using apt-get..."
        sudo apt-get install -y gcc make autoconf automake pkg-config \
            libgnutls28-dev libncurses-dev libxml2-dev zlib1g-dev libgmp-dev texinfo \
            libtree-sitter-dev
        local gcc_ver
        gcc_ver=$(gcc -dumpversion | cut -d. -f1)
        sudo apt-get install -y "libgccjit-${gcc_ver}-dev"
    elif command -v dnf >/dev/null 2>&1; then
        echo "Using dnf..."
        sudo dnf install -y gcc make autoconf automake pkgconfig \
            gnutls-devel ncurses-devel libxml2-devel zlib-devel gmp-devel texinfo \
            libtree-sitter-devel libgccjit-devel
    elif command -v yum >/dev/null 2>&1; then
        echo "Using yum..."
        sudo yum install -y gcc make autoconf automake pkgconfig \
            gnutls-devel ncurses-devel libxml2-devel zlib-devel gmp-devel texinfo \
            libtree-sitter-devel libgccjit-devel
    elif command -v pacman >/dev/null 2>&1; then
        echo "Using pacman..."
        sudo pacman -S --noconfirm gcc make autoconf automake pkgconfig \
            gnutls ncurses libxml2 zlib gmp texinfo \
            tree-sitter
    elif command -v apk >/dev/null 2>&1; then
        echo "Using apk..."
        apk add --no-cache gcc make autoconf automake pkgconfig \
            gnutls-dev ncurses-dev libxml2-dev zlib-dev gmp-dev texinfo \
            tree-sitter-dev
    else
        echo "ERROR: no supported package manager found (tried: apt-get, dnf, yum, pacman, apk)" >&2
        exit 1
    fi
}

# Copy shared libs needed at runtime into $INSTALL_DIR/lib, skipping fundamental glibc/kernel interfaces
copy_runtime_libs() {
    local binary="$1"
    local dest="$2"
    local skip="libpthread\.so|libc\.so|libdl\.so|libm\.so|librt\.so|linux-vdso|ld-linux"
    ldd "$binary" | awk '/=>/ {print $3}' | grep -v '^$' | while read -r lib; do
        echo "$lib" | grep -qE "$skip" && continue
        [ -f "$lib" ] || continue
        cp -Ln "$lib" "$dest/" 2>/dev/null && echo "Bundled: $(basename "$lib")"
    done
}

install_build_deps

BUILD_TMPDIR="$(mktemp -d)"
trap 'rm -rf "$BUILD_TMPDIR"' EXIT

echo "Downloading Emacs ${EMACS_VERSION}..."
curl -fsSL -o "$BUILD_TMPDIR/emacs.tar.xz" "$EMACS_SRC"
tar -xf "$BUILD_TMPDIR/emacs.tar.xz" -C "$BUILD_TMPDIR"

rm -rf "$INSTALL_DIR"
mkdir -p "$INSTALL_DIR"

cd "$BUILD_TMPDIR/emacs-${EMACS_VERSION}"
./configure \
    --prefix="$INSTALL_DIR" \
    --without-x \
    --without-ns \
    --without-dbus \
    --without-gconf \
    --without-gsettings \
    --without-imagemagick \
    --without-gif \
    --without-jpeg \
    --without-png \
    --without-rsvg \
    --without-tiff \
    --without-xpm \
    --with-gnutls \
    --with-xml2 \
    --with-zlib \
    --with-tree-sitter \
    --with-modules \
    --with-native-compilation=aot \
    LDFLAGS="-Wl,-rpath,${INSTALL_DIR}/lib"

make -j"$(nproc)"
make install

# Bundle runtime shared libs into $INSTALL_DIR/lib so the binary is self-contained
mkdir -p "$INSTALL_DIR/lib"
copy_runtime_libs "$INSTALL_DIR/bin/emacs-${EMACS_VERSION}" "$INSTALL_DIR/lib"

mkdir -p "$BIN_DIR"
# Wrapper sets LD_LIBRARY_PATH so bundled libs are preferred at runtime
cat > "$BIN_DIR/emacs" << EOF
#!/bin/sh
export LD_LIBRARY_PATH="${INSTALL_DIR}/lib\${LD_LIBRARY_PATH:+:\$LD_LIBRARY_PATH}"
exec "${INSTALL_DIR}/bin/emacs" "\$@"
EOF
chmod +x "$BIN_DIR/emacs"

cat > "$BIN_DIR/emacsclient" << EOF
#!/bin/sh
exec "${INSTALL_DIR}/bin/emacsclient" "\$@"
EOF
chmod +x "$BIN_DIR/emacsclient"

echo "Emacs ${EMACS_VERSION} installed to ${INSTALL_DIR}"
