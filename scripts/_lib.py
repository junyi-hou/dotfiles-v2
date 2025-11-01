import os
import logging
import shutil
from pathlib import Path


logger = logging.getLogger("dotfiles_installer")

HOME: Path = Path(os.getenv("HOME")).resolve()


def git_root(path: str) -> Path:
    _path = Path(path).resolve()
    if (_path / ".git").exists():
        return _path
    elif path != _path.anchor:
        return git_root(_path.parent.as_posix())
    raise ValueError("Cannot find current Git root")


def get_backup_path(relative_path: Path) -> Path:
    backup_filename = f"{"." if not relative_path.name.startswith(".") else ""}{relative_path.name}.backup"
    return HOME / f".{relative_path.parent}" / backup_filename


def get_target_path(relative_path: Path) -> Path:
    return HOME / f".{relative_path}"


def move(src: Path, dst: Path, dry_run: bool) -> None:
    if dry_run:
        logger.warning(f"moving {src} to {dst}")
        return

    _ = shutil.move(src, dst)
    return


def remove_file(path: Path, dry_run: bool) -> None:
    if dry_run:
        logger.warning(f"removing {path}")
        return

    _ = path.unlink()
    return


def symlink(_from: Path, to: Path, dry_run: bool) -> None:
    if dry_run:
        logger.warning(f"installing {_from} to {to}")
        return

    to.symlink_to(_from)
    return


