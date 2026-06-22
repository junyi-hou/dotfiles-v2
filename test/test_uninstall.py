import tempfile
from pathlib import Path
from unittest.mock import patch
from scripts.uninstall import uninstall


def _make_module_tree(tmp: str) -> tuple[Path, Path]:
    """Returns (module_root, module_file) inside a fake modules/ tree."""
    module_root = Path(tmp).resolve() / "modules"
    module_file = module_root / "mymod" / "rc"
    module_file.parent.mkdir(parents=True)
    module_file.touch()
    return module_root, module_file


def test_uninstall_skips_when_install_path_missing():
    with tempfile.TemporaryDirectory() as tmp:
        module_root, module_file = _make_module_tree(tmp)
        install_root = Path(tmp).resolve() / "home"
        install_root.mkdir()

        with patch("scripts.uninstall.git_root", return_value=Path(tmp).resolve()):
            with patch("scripts._lib.HOME", install_root):
                uninstall(module_file, dry_run=False)


def test_uninstall_skips_broken_symlink_in_module():
    with tempfile.TemporaryDirectory() as tmp:
        module_root = Path(tmp).resolve() / "modules" / "mymod"
        module_root.mkdir(parents=True)
        broken_link = module_root / "rc"
        broken_link.symlink_to("/nonexistent/target")
        install_root = Path(tmp).resolve() / "home"
        install_root.mkdir()

        with patch("scripts.uninstall.git_root", return_value=Path(tmp).resolve()):
            with patch("scripts._lib.HOME", install_root):
                uninstall(broken_link, dry_run=False)


def test_uninstall_removes_symlink_when_installed():
    with tempfile.TemporaryDirectory() as tmp:
        module_root, module_file = _make_module_tree(tmp)
        install_root = Path(tmp).resolve() / "home"
        install_dir = install_root / ".mymod"
        install_dir.mkdir(parents=True)
        install_link = install_dir / "rc"
        install_link.symlink_to(module_file)

        with patch("scripts.uninstall.git_root", return_value=Path(tmp).resolve()):
            with patch("scripts._lib.HOME", install_root):
                uninstall(module_file, dry_run=False)

        assert not install_link.exists()


def test_uninstall_records_mtime_in_state():
    with tempfile.TemporaryDirectory() as tmp:
        module_root, module_file = _make_module_tree(tmp)
        install_root = Path(tmp).resolve() / "home"
        install_dir = install_root / ".mymod"
        install_dir.mkdir(parents=True)
        install_link = install_dir / "rc"
        install_link.symlink_to(module_file)

        state: dict[str, float] = {}
        with patch("scripts.uninstall.git_root", return_value=Path(tmp).resolve()):
            with patch("scripts._lib.HOME", install_root):
                uninstall(module_file, dry_run=False, state=state)

        assert "mymod/rc" in state
        assert isinstance(state["mymod/rc"], float)


def test_uninstall_restores_backup():
    with tempfile.TemporaryDirectory() as tmp:
        module_root, module_file = _make_module_tree(tmp)
        install_root = Path(tmp).resolve() / "home"
        install_dir = install_root / ".mymod"
        install_dir.mkdir(parents=True)
        install_link = install_dir / "rc"
        install_link.symlink_to(module_file)
        backup_path = install_dir / ".rc.backup"
        backup_path.write_text("original content")

        with patch("scripts.uninstall.git_root", return_value=Path(tmp).resolve()):
            with patch("scripts._lib.HOME", install_root):
                uninstall(module_file, dry_run=False)

        assert not install_link.is_symlink()
        assert (install_dir / "rc").read_text() == "original content"


def test_uninstall_skips_non_our_symlink():
    with tempfile.TemporaryDirectory() as tmp:
        module_root, module_file = _make_module_tree(tmp)
        install_root = Path(tmp).resolve() / "home"
        install_dir = install_root / ".mymod"
        install_dir.mkdir(parents=True)
        other_file = Path(tmp) / "other"
        other_file.touch()
        install_link = install_dir / "rc"
        install_link.symlink_to(other_file)

        with patch("scripts.uninstall.git_root", return_value=Path(tmp).resolve()):
            with patch("scripts._lib.HOME", install_root):
                uninstall(module_file, dry_run=False)

        assert install_link.is_symlink()
        assert install_link.resolve() == other_file.resolve()


def test_uninstall_removes_empty_dir():
    with tempfile.TemporaryDirectory() as tmp:
        module_root = Path(tmp).resolve() / "modules"
        module_dir = module_root / "mymod"
        module_dir.mkdir(parents=True)
        child_file = module_dir / "rc"
        child_file.touch()
        install_root = Path(tmp).resolve() / "home"
        install_dir = install_root / ".mymod"
        install_dir.mkdir(parents=True)
        install_link = install_dir / "rc"
        install_link.symlink_to(child_file)

        with patch("scripts.uninstall.git_root", return_value=Path(tmp).resolve()):
            with patch("scripts._lib.HOME", install_root):
                uninstall(module_dir, dry_run=False)

        assert not install_dir.exists()


def test_uninstall_folder_marker_removes_directory_symlink():
    with tempfile.TemporaryDirectory() as tmp:
        module_root = Path(tmp).resolve() / "modules"
        module_dir = module_root / "mymod"
        module_dir.mkdir(parents=True)
        (module_dir / ".install-folder").touch()
        install_root = Path(tmp).resolve() / "home"
        install_root.mkdir()
        install_link = install_root / ".mymod"
        install_link.symlink_to(module_dir)

        with patch("scripts.uninstall.git_root", return_value=Path(tmp).resolve()):
            with patch("scripts._lib.HOME", install_root):
                uninstall(module_dir, dry_run=False)

        assert not install_link.exists()
