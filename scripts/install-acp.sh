#!/usr/bin/env bash

set -euo pipefail

os="$(uname -s)"
arch="$(uname -m)"
t="$os-$arch"
t="$(printf '%s' "$t" | sed -e 's/Darwin-arm64/aarch64-apple-darwin/' -e 's/Darwin-x86_64/x86_64-apple-darwin/' -e 's/Linux-x86_64/x86_64-unknown-linux-gnu/' -e 's/Linux-aarch64/aarch64-unknown-linux-gnu/')"

curl -sSfL -o /tmp/codex-acp.tar.gz "https://github.com/zed-industries/codex-acp/releases/download/v0.16.0/codex-acp-0.16.0-${t}.tar.gz"
tar xzf /tmp/codex-acp.tar.gz -C /tmp
mkdir -p ~/.local/bin
install -m 755 /tmp/codex-acp ~/.local/bin/codex-acp
rm -f /tmp/codex-acp /tmp/codex-acp.tar.gz
