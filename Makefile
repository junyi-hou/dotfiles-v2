.PHONY: install uninstall update-secret build java

install:
	@python -m scripts.install

uninstall:
	@python -m scripts.uninstall

update-secret:
	@python -m scripts.install -m passage

test:
	@pytest ./test_scripts

build:
	@bash ./scripts/install.emacs-macos.sh

java:
	@bash ./scripts/install-java.sh
