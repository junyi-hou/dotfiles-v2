.PHONY: install uninstall update-secret

install:
	@python ./scripts/install.py

uninstall:
	@python ./scripts/uninstall.py

update-secret:
	@python ./scripts/install.py -m passage
