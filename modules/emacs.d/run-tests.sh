#!/usr/bin/env bash
cd "$(dirname "$0")"

emacs --batch \
  -l early-init.el \
  --eval '(elpaca-wait)' \
  -l init.el \
  --eval '(elpaca-wait)' \
  -l ert \
  --eval '(advice-add #'"'"'gatsby>>update-elpaca-lock-file :override #'"'"'ignore)' \
  --eval '(add-to-list (quote load-path) (expand-file-name "tests"))' \
  $(for f in tests/*.el; do echo "-l $f"; done) \
  -f ert-run-tests-batch-and-exit
