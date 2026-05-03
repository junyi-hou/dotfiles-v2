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
                # install_path does not exist — should not raise
                uninstall(module_file, dry_run=False)


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
