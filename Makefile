.PHONY: install uninstall update-secret build java

install:
	@python -m scripts.install

uninstall:
	@python -m scripts.uninstall

update-secret:
	@python -m scripts.install -m passage

test-scripts:
	@pytest ./test_scripts

test-emacs:
	@bash ./modules/emacs.d/run-tests.sh

build:
	@bash ./scripts/install-emacs-macos.sh

java:
	@python -m scripts.install_java

claude:
	@python -m scripts.install -m claude profile bash_profile zprofile
	@python -m scripts.install_claude
