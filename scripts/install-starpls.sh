#!/usr/bin/env bash

set -euo pipefail

version="0.1.22"
os="$(uname -s)"
arch="$(uname -m)"

case "${os}-${arch}" in
  Darwin-arm64)  asset="starpls-darwin-arm64" ;;
  Darwin-x86_64) asset="starpls-darwin-amd64" ;;
  Linux-aarch64) asset="starpls-linux-aarch64" ;;
  Linux-x86_64)  asset="starpls-linux-amd64" ;;
  *) echo "Unsupported platform: ${os}-${arch}" >&2; exit 1 ;;
esac

curl -sSfL -o /tmp/starpls "https://github.com/withered-magic/starpls/releases/download/v${version}/${asset}"
mkdir -p ~/.local/bin
install -m 755 /tmp/starpls ~/.local/bin/starpls
rm -f /tmp/starpls
