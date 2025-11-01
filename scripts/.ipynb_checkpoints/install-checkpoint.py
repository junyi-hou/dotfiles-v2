#!/usr/bin/env python3

import argparse
import shutil
import os
import logging
import sys

from pathlib import Path

logger = logging.getLogger("dotfiles_installer")
logger.setLevel(logging.INFO)
handler = logging.StreamHandler(sys.stdout)
handler.setLevel(logging.INFO)
handler.setFormatter(logging.Formatter("[%(levelname)s] %(message)s"))
logger.addHandler(handler)


HOME: Path = pathlib.Path(os.getenv("HOME")).resolve()


def git_root(path: str) -> Path:
    _path = Path(path).resolve()
    if (_path / ".git").exists():
        return _path
    elif path != _path.anchor:
        return git_root(_path.parent.as_posix())
    raise ValueError("Cannot find current ")

def _move(src: Path, dst: Path, dry_run: bool) -> None:
    if dry_run:
        logger.info(f"moving {src} to {dst}")
        return

    _ = shutil.move(src, dst)
    return


def _symlink(_from: Path, to: Path, dry_run: bool) -> None:
    if dry_run:
        logger.info(f"installing {_from} to {to}")
        return

    to.symlink_to(_from)
    return


def install(module: str | Path, *, dry_run: bool = True) -> None:
    """
    Install MODULE by symlinking it to the corresponding position in current user's $HOME directory.
    If MODULE is an immediate child of the repo root, symlink it to
    """

    # _root = git_root(__file__)
    _root = git_root(".")
    if isinstance(module, str):
        module = _root / module

    # deal with symlinks
    if module.is_symlink():
        module = module.resolve()

    relative_path = module.relative_to(_root)

    logger.info(f"Installing {relative_path} ...")

    if module.is_file():
        target = HOME / f".{relative_path}"
        if target.exists():
            logger.info(f"Found existing files in {target}, moving...")
            target_backup = (
                target.parent / f"{"." if not target.name.startswith(".") else ""}{target.name}"
            )
            _move(target, target_backup, dry_run)

        _symlink(module, target, dry_run)
        logger.info(f"Module {relative_path} installed")

    elif module.is_dir():
        logger.info(f"Recursively installing {relative_path} ...")
        for file in module.iterdir():
            install(file)

    else:
        raise ValueError(f"{module} is neither file nor directory.")
    

def main() -> int:
    return 0
