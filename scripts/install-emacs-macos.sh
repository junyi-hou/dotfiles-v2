set -euo pipefail

command -v brew > /dev/null || {
    echo "`brew` not found, install it first."
    exit 1
}

brew install gcc pkgconf texinfo autoconf libgccjit librsvg

## until emacs adopts tree-sitter ABI 26
brew install tree-sitter@0.25
brew link --overwrite tree-sitter@0.25

git clone -b emacs-mac-gnu_master_exp https://github.com/jdtsmith/emacs-mac.git
cd emacs-mac
git checkout df4e43d51c36275c4d6c133b44e7789d08d7de59

# Apply patches if they exist
if [ -d "../patches" ]; then
    echo "Applying patches from ../patches directory..."
    for patch_file in ../patches/*.patch; do
        if [ -f "$patch_file" ]; then
            echo "Applying patch: $(basename "$patch_file")"
            git apply "$patch_file"
        fi
    done
else
    echo "No patches directory found, skipping patch application."
fi

./autogen.sh

NATIVE_COMP_FLAG="--with-native-compilation"
if [ "${CI:-}" = "true" ]; then
    NATIVE_COMP_FLAG=""
fi

CFLAGS="-O2 -mcpu=native -I/opt/homebrew/Cellar/libgccjit/15.2.0/include" \
LDFLAGS="-I/opt/homebrew/Cellar/libgccjit/15.2.0/include -L-I/opt/homebrew/Cellar/libgccjit/15.2.0/lib/gcc/15" \
LIBRARY_PATH="/opt/homebrew/Cellar/gcc/15.2.0/lib/gcc/15" \
./configure $NATIVE_COMP_FLAG --with-tree-sitter --enable-mac-app=yes --enable-mac-self-contained
make -j10 && make install

# install emacs runnable
mkdir -p "$HOME/.local/bin"
ln -sf "/Applications/Emacs.app/Contents/MacOS/Emacs.sh" "$HOME/.local/bin/emacs"
ln -sf "/Applications/Emacs.app/Contents/MacOS/bin/emacsclient" "$HOME/.local/bin/emacsclient"
