.PHONY: install uninstall build java claude test test-emacs

install:
	@python -m scripts.uninstall
	@python -m scripts.install

uninstall:
	@python -m scripts.uninstall

test:
	@pytest ./test

test-emacs:
	@bash ./modules/emacs.d/run-tests.sh

build:
	@bash ./scripts/install-emacs-macos.sh

java:
	@python -m scripts.install_java

claude:
	@python -m scripts.install -m claude profile bash_profile zprofile config
	@python -m scripts.install_claude
