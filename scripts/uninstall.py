#!/usr/bin/env python3

import argparse
import logging

from pathlib import Path
from typing import cast
from dataclasses import dataclass

logger = logging.getLogger("dotfiles_installer")
logger.setLevel(logging.DEBUG)

from _lib import git_root, move, get_backup_path, get_target_path, remove_file


AVAILABLE_MODULES: list[str] = [m.name for m in (git_root(__file__) / "modules").iterdir()]


def uninstall(module: str | Path, *, dry_run: bool = True) -> None:
    """
    """
    module_root = git_root(__file__) / "modules"

    if isinstance(module, str):
        module = module_root / module

    # deal with symlinks
    if module.is_symlink():
        module = module.resolve()

    # catch unexpected
    if not (module.is_dir() or module.is_file()):
        raise ValueError(f"{module} is neither file nor directory.")

    relative_path = module.relative_to(module_root)
    install_path = get_target_path(relative_path)
    backup_path = get_backup_path(install_path)

    logger.debug(f"Uninstalling {relative_path} ...")
    
    if module.is_file():
        # if module is file, install_path must be symlink
        # make sure that install_path is what we have installed!
        if install_path.is_symlink() and install_path.resolve() == module:
            remove_file(install_path, dry_run)
        else:
            logger.debug(f"{relative_path} was not installed by us, leave it there...")

    elif module.is_dir():
        # go into install_path
        logger.debug(f"Recursively uninstalling {relative_path} ...")
        for child in module.iterdir():
            uninstall(child, dry_run=dry_run)

        # if module is dir, install_path must be dir
        if next(install_path.iterdir(), None) is None:
            logger.debug(f"{install_path} is empty, removing...")
            install_path.rmdir()

    # restore backup file
    if backup_path.exists():
        logger.debug(f"Backup file found, restoring {relative_path} ...")
        move(backup_path, install_path, dry_run)
    logger.debug(f"Module {relative_path} uninstalled!")


def main() -> int:

    @dataclass
    class Arguments:
        dry_run: bool
        modules: list[str]

    parser = argparse.ArgumentParser()
    _ = parser.add_argument(
        "--modules", "-m",
        nargs="+",
        choices=AVAILABLE_MODULES,
        default=AVAILABLE_MODULES,
        help="modules to install into $HOME.",
    )
    _ = parser.add_argument(
        "--dry_run", "-d",
        action="store_true",
        help="echo the command, instead of running it."
    )

    args, _ = parser.parse_known_args()
    args = cast(Arguments, cast(object, args))

    for module in args.modules:
        uninstall(module, dry_run=args.dry_run)

    return 0


if __name__ == "__main__":
    main()
