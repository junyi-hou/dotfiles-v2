## Description

Emacs configuration using [elpaca](https://github.com/progfolio/elpaca) as the package manager and `use-package` for package configuration.

## File naming

- `gatsby>` prefix: public symbols (functions, variables, commands)
- `gatsby>>` prefix: private/internal symbols
- `lisp/gatsby>NAME.el`: one file per concern (e.g. `gatsby>editing.el`, `gatsby>lsp.el`)
- `lisp/gatsby>>utility.el`: shared macros and utility functions, loaded first

## Running tests

```sh
./run-tests.sh
```

Tests live in `tests/` and use ERT. Each `lisp/gatsby>NAME.el` has a corresponding `tests/gatsby-NAME-test.el`.

## Package pinning

`elpaca-lock.el` records pinned package recipes. It is auto-updated as a build step (`gatsby>>elpaca-update-lock-file` is appended to `elpaca-default-build-steps`) whenever a package is built or installed. On startup, `elpaca-menu-lock-file` reads it to pin versions. Do not edit it by hand.
