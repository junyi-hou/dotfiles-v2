#!/usr/bin/env python3

import argparse
import logging
from pathlib import Path
from typing import cast
from dataclasses import dataclass

from _lib import logger, git_root, symlink, move, get_target_path, get_backup_path


AVAILABLE_MODULES: list[str] = [m.name for m in (git_root(__file__) / "modules").iterdir()]

def install(module: str | Path, *, dry_run: bool = True) -> None:
    """
    Install MODULE by symlinking it to the corresponding position in the current user's $HOME directory.
    If MODULE is an immediate child of the repo root, symlink it to its target location.
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

    logger.info(f"Installing {relative_path} ...")

    if install_path.exists():

        if install_path.resolve() == module.resolve() and not dry_run:
            logger.info(f"{relative_path} already installed, skipping")
            return

        if not install_path.is_dir():
            logger.debug(f"{install_path} exists, moving...")
            move(install_path, backup_path, dry_run)

    if module.is_file():
        symlink(module, install_path, dry_run)
        if not dry_run:
            logger.info(f"Module {relative_path} installed!")

    elif module.is_dir():
        if dry_run:
            logger.info(f"Would create directory {install_path}")
        else:
            install_path.mkdir(parents=True, exist_ok=True)
        logger.debug(f"Recursively installing {module} ...")
        for child in module.iterdir():
            install(child, dry_run=dry_run)
    

def main() -> int:
    """
    Entry point for the installer. Parses arguments and installs requested modules.
    """
    @dataclass
    class Arguments:
        dry_run: bool
        verbose: bool
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
    _ = parser.add_argument(
        "--verbose", "-v",
        action="store_true",
        help="show debug level log messages."
    )

    args = parser.parse_args()
    args = cast(Arguments, cast(object, args))

    if args.verbose:
        logger.setLevel(logging.DEBUG)

    for module in args.modules:
        install(module, dry_run=args.dry_run)

    logger.info("Installation Finishes!")

    return 0

if __name__ == "__main__":
    main()
