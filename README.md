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

Linux is supported via a headless Emacs AppImage. Run `make build` to install.

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

Remote machines are set up via the `deploy` script, which clones the dotfiles repo:

```sh
deploy user@host [-p port]
```

After setup, connect via TRAMP in Emacs (`C-x C-f /ssh:user@host:/path`).

With additional `-b` flag, we will do `make build` + `make install` in the remote server as well:
```sh
deploy -b user@host [-p port]
```

With `--use-local-copy`, the local dotfiles directory is copied to the remote via `rsync` instead of cloning from GitHub:
```sh
deploy --use-local-copy user@host [-p port]
```

### Remote Agent

Start a headless agent on a remote machine from your laptop using [agent-shell-to-go](https://github.com/junyi-hou/agent-shell-to-go), which allows you to keep interacting with a code agent from discord/slack on your phone.

To start a remote agent, run
```sh
run-agent /path/to/project user@host
```

This SSHes into `user@host` and launches a headless Emacs session that communicates via Slack or Discord. Configure credentials in `~/dotfiles-v2/modules/claude/config.el` on the remote before running.

Prerequisites on the remote:
- Dotfiles deployed (see [Remote Development](#remote-development))
- Dotfiles fully installed (`make build` + `make install`, now can be automated with adding `-b` flag to the `deploy` script)

The agent runs in the background using `emacs --daemon` and persists after the SSH connection closes.


## Secrets

Secrets are stored in `env.json.enc` (a SOPS-encrypted JSON file, not checked into git) and managed via [sops](https://github.com/getsops/sops) with an age key at `~/.config/age/key`. Set `SOPS_AGE_KEY_FILE=~/.config/age/key` in your shell environment (done automatically via `modules/profile`).

A CLI tools are available on any machine with secrets deployed:

- `passage [key/path]` — print a secret value (or full JSON if no path given)

### Updating secrets

Secrets can only be edited on the local machine. This is intentional: each remote machine holds its own re-encrypted copy of the secrets, so edits on a remote would diverge and never propagate back. The local machine is the single source of truth.

```
M-x sops-edit-secret
```

Decrypts into a scratch buffer. `C-c C-c` re-encrypts and saves; `C-c C-k` cancels. `sops-retrieve-secret` lets you pick a secret by path and copies it to the clipboard (cleared after 30s).

## Planned Features

[ ] devcontainer for agents
[ ] self-managed plugins (via git submodules) - available if [this issue](https://github.com/anthropics/claude-agent-sdk-typescript/issues/141) and [this PR](https://github.com/anthropics/claude-agent-sdk-python/pull/803) lands.
[ ] simple side-by-side diff review (ediff is too heavy, and I just want something simpler)
[ ] remote `run-agent` rebuild
