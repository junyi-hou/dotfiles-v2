#!/usr/bin/env bash
set -euo pipefail

# Navigate to project root
# do not rely on `git rev-parse --show-toplevel` since we may not be in a git repo
cd "$JUPYTER_PATH/../.."

CLASSPATH=$(clj -A:repl -Spath)

exec java -Djava.awt.headless=true \
  -cp "$CLASSPATH" \
  clojure.main -m clojupyter.kernel.core \
  "$@"
