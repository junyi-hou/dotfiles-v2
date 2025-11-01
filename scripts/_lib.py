import os
import logging
import shutil
from pathlib import Path


logger = logging.getLogger("dotfiles_installer")

HOME: Path = Path.home()


def git_root(path: str) -> Path:
    """
    Find the root directory of the Git repository in PATH.

    Recursively searches parent directories starting from the given path until 
    finding a directory containing a '.git' folder. If no Git root is found, 
    raises a ValueError.

    Args:
        path (str): Starting path for the search (can be any file or directory)

    Returns:
        Path: Absolute path to the Git repository root

    Raises:
        ValueError: If no .git folder is found in any parent directory
    """
    _path = Path(path).resolve()
    if (_path / ".git").exists():
        return _path
    elif path != _path.anchor:
        return git_root(_path.parent.as_posix())
    raise ValueError("Cannot find current Git root")


def get_backup_path(install_path: Path) -> Path:
    """
    Generate a backup path for an existing install target.

    Creates a backup filename matching the install_path name, adding a preceding 
    dot if the original filename is not hidden. The backup filename will have 
    the format: [`.`]name[.backup] 

    Args:
        install_path (Path): Path of the target being installed

    Returns:
        Path: Path for the backup file (same directory as install_path)
    """
    prefix = "." if not install_path.name.startswith(".") else ""
    backup_filename = f"{prefix}{install_path.name}.backup"

    return install_path.parent / backup_filename


def get_target_path(relative_path: Path) -> Path:
    """
    Resolve the absolute installation path for a dotfile.

    Converts a repository-relative path to the target installation path by:
    1. Adding a leading dot to the filename
    2. Placing it under the user's home directory

    Args:
        relative_path (Path): Path relative to the dotfiles module root

    Returns:
        Path: Absolute target path in the home directory
    """
    return HOME / f".{relative_path}"


def move(src: Path, dst: Path, dry_run: bool) -> None:
    """
    Move a file/directory with dry-run capability.

    In dry-run mode, logs the intended move without modifying filesystem. 
    In execution mode, uses shutil.move for safe move operations that support 
    cross-device transfers.

    Args:
        src (Path): Source path to move
        dst (Path): Destination path
        dry_run (bool): Simulation mode flag
    """
    if dry_run:
        logger.warning(f"moving {src} to {dst}")
        return

    _ = shutil.move(src, dst)
    return


def remove_file(path: Path, dry_run: bool) -> None:
    """
    Remove a file with dry-run capability.

    In dry-run mode, logs the intended removal. In execution mode, 
    deletes the file using Path.unlink().

    Args:
        path (Path): File to remove
        dry_run (bool): Simulation mode flag
    """
    if dry_run:
        logger.warning(f"removing {path}")
        return

    _ = path.unlink()
    return


def symlink(from_: Path, to: Path, dry_run: bool) -> None:
    """
    Create a symbolic link with dry-run capability.

    In dry-run mode, logs the intended symlink creation. In execution mode, 
    creates a symbolic link at 'to' pointing to 'from_'. Both paths must be 
    absolute.

    Args:
        from_ (Path): Source file/directory the link points to
        to (Path): Link location to create
        dry_run (bool): Simulation mode flag
    """
    if dry_run:
        logger.warning(f"installing {from_} to {to}")
        return

    to.symlink_to(from_)
    return


