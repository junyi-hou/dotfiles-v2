import pytest
from pathlib import Path
from unittest.mock import patch, MagicMock
from scripts.install import symlink_project_root
from scripts.uninstall import remove_project_root_symlink

@patch("scripts.install.git_root")
@patch("scripts.install.Path.home")
@patch("scripts.install.symlink")
def test_symlink_project_root_creates_link(mock_symlink, mock_home, mock_git_root):
    mock_git_root.return_value = Path("/repo/root")
    mock_home_path = MagicMock(spec=Path)
    mock_home.return_value = mock_home_path

    mock_dotfiles_v2 = MagicMock(spec=Path)
    mock_dotfiles_v2.exists.return_value = False
    mock_home_path.__truediv__.return_value = mock_dotfiles_v2

    symlink_project_root(dry_run=False)

    mock_symlink.assert_called_once_with(Path("/repo/root"), mock_dotfiles_v2, dry_run=False)

@patch("scripts.install.git_root")
@patch("scripts.install.Path.home")
@patch("scripts.install.symlink")
def test_symlink_project_root_skips_if_exists(mock_symlink, mock_home, mock_git_root):
    mock_git_root.return_value = Path("/repo/root")
    mock_home_path = MagicMock(spec=Path)
    mock_home.return_value = mock_home_path

    mock_dotfiles_v2 = MagicMock(spec=Path)
    mock_dotfiles_v2.exists.return_value = True
    mock_home_path.__truediv__.return_value = mock_dotfiles_v2

    symlink_project_root(dry_run=False)

    mock_symlink.assert_not_called()

@patch("scripts.uninstall.git_root")
@patch("scripts.uninstall.Path.home")
@patch("scripts.uninstall.remove_file")
def test_remove_project_root_symlink_deletes_link(mock_remove_file, mock_home, mock_git_root):
    mock_git_root.return_value = Path("/repo/root")
    mock_home_path = MagicMock(spec=Path)
    mock_home.return_value = mock_home_path

    mock_dotfiles_v2 = MagicMock(spec=Path)
    mock_dotfiles_v2.is_symlink.return_value = True
    mock_dotfiles_v2.resolve.return_value = Path("/repo/root")
    mock_home_path.__truediv__.return_value = mock_dotfiles_v2

    remove_project_root_symlink(dry_run=False)

    mock_remove_file.assert_called_once_with(mock_dotfiles_v2, dry_run=False)

@patch("scripts.uninstall.git_root")
@patch("scripts.uninstall.Path.home")
@patch("scripts.uninstall.remove_file")
def test_remove_project_root_symlink_skips_if_not_symlink(mock_remove_file, mock_home, mock_git_root):
    mock_git_root.return_value = Path("/repo/root")
    mock_home_path = MagicMock(spec=Path)
    mock_home.return_value = mock_home_path

    mock_dotfiles_v2 = MagicMock(spec=Path)
    mock_dotfiles_v2.is_symlink.return_value = False
    mock_home_path.__truediv__.return_value = mock_dotfiles_v2

    remove_project_root_symlink(dry_run=False)

    mock_remove_file.assert_not_called()
