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

Secrets are stored in `env.json.enc` (a SOPS-encrypted JSON file) and managed via [sops](https://github.com/getsops/sops) with an age key, stored at `keys/age`. The `SOPS_AGE_KEY_FILE` environment variable is set to `$HOME/dotfiles-v2/keys/age` automatically via `modules/profile`.

## Interacting with Secrets

Two scripts are provided in `modules/local/bin/`:

- `passage [key/path]` — decrypt and print a secret value (or the full JSON if no path given). An emacs command `sops-retrieve-secret` takes the key path to a secret and return the secret vaule. If called interactively, it asks users to pick from the secret list and copy the secret value to the clipboard (clean after 30s).
- `run-with-env [ENV...] -- <command>` — run a command with secrets injected as environment variables; omit `ENV...` to inject all secrets.

To edit secrets:
```
M-x sops-edit-secret
```
