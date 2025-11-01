# Personal Dotfiles Repo

## Dependency Installation

### Mac OS

First install xcode:
```
xcode-select --install
```

Then install homebrew:
```
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
```

Build Emacs-mac from [the source](https://github.com/jdtsmith/emacs-mac):
```
brew install gcc pkgconf texinfo autoconf tree-sitter libgccjit librsvg

cd ~ && mkdir -p tmp && cd ~/tmp
git clone https://github.com/jdtsmith/emacs-mac.git
cd emacs-mac
./autogen.sh
CFLAGS="-O2 -mcpu=native -I/opt/homebrew/Cellar/libgccjit/15.2.0/include" \
LDFLAGS="-I/opt/homebrew/Cellar/libgccjit/15.2.0/include -L-I/opt/homebrew/Cellar/libgccjit/15.2.0/lib/gcc/15" \
LIBRARY_PATH="/opt/homebrew/Cellar/gcc/15.2.0/lib/gcc/15 \
./configure --with-native-compilation --with-tree-sitter --enable-mac-app=yes --enable-mac-self-contained
make -j10 && make install
```

Finally, install direnv for local environment
```
brew install direnv
```

### Linux

TODO

## Install Configuration

Clone this repo:
```
git clone <TBD> /path/to/repo
cd /path/to/repo
make install
```

## Setup Secrets
