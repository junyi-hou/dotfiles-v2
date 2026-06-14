.PHONY: install uninstall build java test test-emacs

install:
	@python -m scripts.uninstall
	@python -m scripts.install
	@if [ "$$(uname -s)" = "Darwin" ]; then bash ./scripts/macos-defaults.sh; fi

uninstall:
	@python -m scripts.uninstall

test:
	@pytest ./test

test-emacs:
	@bash ./modules/emacs.d/run-tests.sh

build:
	@case "$$(uname -s)" in \
		Darwin) bash ./scripts/install-emacs-macos.sh ;; \
		Linux)  bash ./scripts/install-emacs-linux.sh ;; \
		*) echo "Unsupported OS: $$(uname -s)"; exit 1 ;; \
	esac

java:
	@python -m scripts.install_java

