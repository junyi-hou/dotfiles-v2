#!/usr/bin/env python3

import argparse
import atexit
import json
import logging
import subprocess
from pathlib import Path
from typing import cast
from dataclasses import dataclass, field

from ._lib import logger, git_root, symlink, move, get_target_path, get_backup_path


AVAILABLE_MODULES: list[str] = [
    m.name for m in (git_root(__file__) / "modules").iterdir()
]

INSTALL_FOLDER_MARKER = ".install-folder"
SKIPPED_INSTALL_FILES = {".gitignore"}


def should_install_folder(module: Path) -> bool:
    return module.is_dir() and (module / INSTALL_FOLDER_MARKER).exists()


def is_git_ignored(path: Path, repo_root: Path) -> bool:
    result = subprocess.run(
        ["git", "check-ignore", "--no-index", "--quiet", str(path.relative_to(repo_root))],
        cwd=repo_root,
        check=False,
        stderr=subprocess.DEVNULL,
    )
    return result.returncode == 0


def should_skip_install(module: Path, repo_root: Path) -> bool:
    if module.name in SKIPPED_INSTALL_FILES:
        return True

    return is_git_ignored(module, repo_root)


def install(module: str | Path, *, dry_run: bool = True, state: dict[str, float] | None = None) -> None:
    """
    Install MODULE by symlinking it to the corresponding position in the current user's $HOME directory.
    If MODULE is an immediate child of the repo root, symlink it to its target location.
    """
    repo_root = git_root(__file__)
    module_root = repo_root / "modules"

    if isinstance(module, str):
        module = module_root / module

    if should_skip_install(module, repo_root):
        logger.debug(f"Skipping {module.relative_to(module_root)}")
        return

    # deal with symlinks
    if module.is_symlink():
        module = module.resolve()

    # catch unexpected
    if not (module.is_dir() or module.is_file()):
        raise ValueError(f"{module} is neither file nor directory.")

    relative_path = module.relative_to(module_root)
    install_path = get_target_path(relative_path)
    backup_path = get_backup_path(install_path)
    install_folder = should_install_folder(module)

    logger.debug(f"Installing {relative_path} ...")

    if install_path.exists():
        if install_path.resolve() == module.resolve():
            logger.debug(f"{relative_path} already installed, skipping")
            return

        if install_folder or not install_path.is_dir():
            logger.debug(f"{install_path} exists, moving...")
            move(install_path, backup_path, dry_run)

    if module.is_file() or install_folder:
        symlink(module, install_path, dry_run)
        if state is not None and str(relative_path) in state:
            logger.debug(f"{relative_path} reinstalled, no changes")
        else:
            logger.info(f"Module {relative_path} installed!")

    elif module.is_dir():
        if dry_run:
            logger.debug(f"Would create directory {install_path}")
        else:
            install_path.mkdir(parents=True, exist_ok=True)
        logger.debug(f"Recursively installing {module} ...")
        for child in module.iterdir():
            install(child, dry_run=dry_run, state=state)


def symlink_project_root(dry_run: bool = True) -> None:
    """
    Symlink project root to ~/dotfiles-v2 if it does not exist.
    """
    root = git_root(__file__)
    dotfiles_v2 = Path.home() / "dotfiles-v2"
    if not dotfiles_v2.exists():
        symlink(root, dotfiles_v2, dry_run=dry_run)
        logger.info(f"Symlinked {root} to {dotfiles_v2}")


def main() -> int:
    """
    Entry point for the installer. Parses arguments and installs requested modules.
    """

    @dataclass
    class Arguments:
        dry_run: bool
        verbose: bool
        state_file: str | None
        modules: list[str] = field(default_factory=list)

    parser = argparse.ArgumentParser()
    _ = parser.add_argument(
        "--modules",
        "-m",
        nargs="+",
        choices=AVAILABLE_MODULES,
        default=AVAILABLE_MODULES,
        help="modules to install into $HOME.",
    )
    _ = parser.add_argument(
        "--dry_run",
        "-d",
        action="store_true",
        help="echo the command, instead of running it.",
    )
    _ = parser.add_argument(
        "--verbose", "-v", action="store_true", help="show debug level log messages."
    )
    _ = parser.add_argument(
        "--state-file", "-s",
        default=None,
        help="read uninstalled file paths from this JSON file to detect new modules.",
    )

    args = parser.parse_args()
    args = cast(Arguments, cast(object, args))

    # Automatically enable verbose mode when dry_run is enabled
    if args.dry_run:
        args.verbose = True

    if args.verbose:
        logger.setLevel(logging.DEBUG)

    state: dict[str, float] | None = None
    if args.state_file:
        state_path = Path(args.state_file)
        atexit.register(state_path.unlink, missing_ok=True)
        if state_path.exists():
            state = json.loads(state_path.read_text())

    # Symlink project root to ~/dotfiles-v2
    symlink_project_root(dry_run=args.dry_run)

    for module in args.modules:
        install(module, dry_run=args.dry_run, state=state)

    logger.info("Installation Finishes!")

    return 0


if __name__ == "__main__":
    main()
