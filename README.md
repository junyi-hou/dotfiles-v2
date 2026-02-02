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

Finally, install dependencies for emacs packages
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
make build    # this builds & installs emacs-mac
make install  # places dotfiles to appropriate locations
```

## Setup Secrets

Install dependencies:
```
brew install gnu-getopt
```

Interacting with secrets are handled via `passage`. To use an existing key (so that the secrets in `modules/passage/store` is usable), copy the (r)age key to `modules/passage/identities`. To use a new key, run
```
rage-keygen >> modules/passage/identities
```

## Add New Secrets

If you wants to create new secrets, make sure to do it inside of this repo, so that the new secret is added to `modules/passage/store` instead of the default `$HOME/.passage/store`. After creating the secret, install it with `make update-secret`.

In the future I should create a function to automate this process.