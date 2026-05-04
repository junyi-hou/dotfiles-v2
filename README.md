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

Support for linux is pending. But development on a remote linux box is supported (see [this section](#remote-development))

## Install Configuration

Clone this repo:
```
git clone https://github.com/junyi-hou/dotfiles-v2 /path/to/repo
cd /path/to/repo/dotfiles-v2
make build    # this builds & installs emacs-mac
make install  # places dotfiles to appropriate locations

make claude  # install claude-code and acp integration
make java    # install java (for clojure/scala development)
```

## Remote Development

Remote machines are set up via the `deploy` script, which clones the dotfiles repo and provisions secrets in one step:

```sh
deploy user@host [-p port]
```

After setup, connect via TRAMP in Emacs (`C-x C-f /ssh:user@host:/path`). The remote environment will have the full dotfiles configuration including secrets available via `passage` and `run-with-env`.

## Secrets

Secrets are stored in `env.json.enc` (a SOPS-encrypted JSON file, not checked into git) and managed via [sops](https://github.com/getsops/sops) with an age key at `~/.config/age/key`. Set `SOPS_AGE_KEY_FILE=~/.config/age/key` in your shell environment (done automatically via `modules/profile`).

Two CLI tools are available on any machine with secrets deployed:

- `passage [key/path]` — print a secret value (or full JSON if no path given)
- `run-with-env [ENV...] -- <command>` — run a command with secrets injected as env vars; omit `ENV...` to inject all

### Updating secrets

Secrets can only be edited on the local machine. This is intentional: each remote machine holds its own re-encrypted copy of the secrets, so edits on a remote would diverge and never propagate back. The local machine is the single source of truth.

```
M-x sops-edit-secret
```

Decrypts into a scratch buffer. `C-c C-c` re-encrypts and saves; `C-c C-k` cancels. `sops-retrieve-secret` lets you pick a secret by path and copies it to the clipboard (cleared after 30s).

After editing, push the updated secrets to each remote:

```sh
deploy --secret-only user@host
```

To rotate after a key compromise: change the actual secret values via `sops-edit-secret`, then re-run `deploy --secret-only` for each affected machine.
