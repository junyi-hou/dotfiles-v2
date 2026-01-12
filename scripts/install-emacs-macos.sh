set -euo pipefail

command -v brew > /dev/null || {
    echo "`brew` not found, install it first."
    exit 1
}

brew install gcc pkgconf texinfo autoconf libgccjit librsvg

## until emacs adopts tree-sitter ABI 26
brew install tree-sitter@0.25
brew link --overwrite tree-sitter@0.25

git clone -b emacs-mac-30_1_exp https://github.com/jdtsmith/emacs-mac.git
cd emacs-mac

# Apply patches if they exist
## FIXME - this does not work
# if [ -d "../patches" ]; then
#     echo "Applying patches from ../patches directory..."
#     for patch_file in ../patches/*.patch; do
#         if [ -f "$patch_file" ]; then
#             echo "Applying patch: $(basename "$patch_file")"
#             git apply "$patch_file"
#         fi
#     done
# else
#     echo "No patches directory found, skipping patch application."
# fi

./autogen.sh

CFLAGS="-O2 -mcpu=native -I/opt/homebrew/Cellar/libgccjit/15.2.0/include" \
LDFLAGS="-I/opt/homebrew/Cellar/libgccjit/15.2.0/include -L-I/opt/homebrew/Cellar/libgccjit/15.2.0/lib/gcc/15" \
LIBRARY_PATH="/opt/homebrew/Cellar/gcc/15.2.0/lib/gcc/15" \
./configure --with-native-compilation --with-tree-sitter --enable-mac-app=yes --enable-mac-self-contained
make -j10 && make install
