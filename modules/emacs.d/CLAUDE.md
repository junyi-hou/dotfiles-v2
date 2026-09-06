## Description

Emacs configuration using [elpaca](https://github.com/progfolio/elpaca) as the package manager and `use-package` for package configuration.

## File Naming

- `gatsby>` prefix: public symbols (functions, variables, commands)
- `gatsby>>` prefix: private/internal symbols
- `lisp/gatsby-NAME.el`: one file per concern (e.g. `gatsby-editing.el`, `gatsby-lsp.el`)
- `lisp/gatsby--utility.el`: shared macros and utility functions, loaded first

## Running Tests

```sh
./run-tests.sh
```

Tests live in `tests/` and use `ert`. Each `lisp/gatsby-NAME.el` has corresponding `tests/gatsby-NAME-test.el`.

## Formatting

Format every edit to Elisp files before you run tests. Use `elisp-autofmt` through the running Emacs server. Format only the region you changed. Do not reformat whole legacy files, because that creates large diffs.

```sh
emacsclient -e '
  (with-current-buffer (find-file-noselect "PATH/TO/FILE.el")
    (elisp-autofmt-region BEG END)
    (save-buffer))'
```

## Package Pinning

`elpaca-lock.el` records pinned package recipes. It is auto-updated as a build step (`gatsby>>elpaca-update-lock-file` is appended to `elpaca-default-build-steps`) whenever a package is built or installed. On startup, `elpaca-menu-lock-file` reads it to pin versions. Do not edit it by hand.

## Development

- Go to `~/.emacs.d/elpaca/builds` to find source code of installed packages.
- Use `emacsclient` to access a running Emacs instance.
