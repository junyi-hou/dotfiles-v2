import tempfile
from pathlib import Path
from unittest.mock import patch
from scripts.install import install


def _make_module_tree(tmp: str) -> tuple[Path, Path]:
    """Returns (module_root, module_file) inside a fake modules/ tree."""
    module_root = Path(tmp).resolve() / "modules"
    module_file = module_root / "mymod" / "rc"
    module_file.parent.mkdir(parents=True)
    module_file.touch()
    return module_root, module_file


def test_install_creates_parent_dir():
    with tempfile.TemporaryDirectory() as tmp:
        module_root, module_file = _make_module_tree(tmp)
        install_root = Path(tmp).resolve() / "home"
        install_root.mkdir()

        with patch("scripts.install.git_root", return_value=Path(tmp).resolve()):
            with patch("scripts._lib.HOME", install_root):
                install(module_file, dry_run=False)

        install_link = install_root / ".mymod" / "rc"
        assert install_link.is_symlink()
        assert install_link.resolve() == module_file.resolve()


def test_install_skips_if_already_installed():
    with tempfile.TemporaryDirectory() as tmp:
        module_root, module_file = _make_module_tree(tmp)
        install_root = Path(tmp).resolve() / "home"
        install_dir = install_root / ".mymod"
        install_dir.mkdir(parents=True)
        install_link = install_dir / "rc"
        install_link.symlink_to(module_file)

        with patch("scripts.install.git_root", return_value=Path(tmp).resolve()):
            with patch("scripts._lib.HOME", install_root):
                install(module_file, dry_run=False)

        assert install_link.is_symlink()
        assert install_link.resolve() == module_file.resolve()
def test_install_backs_up_existing_file():
    with tempfile.TemporaryDirectory() as tmp:
        module_root, module_file = _make_module_tree(tmp)
        install_root = Path(tmp).resolve() / "home"
        install_dir = install_root / ".mymod"
        install_dir.mkdir(parents=True)
        existing_file = install_dir / "rc"
        existing_file.write_text("original content")

        with patch("scripts.install.git_root", return_value=Path(tmp).resolve()):
            with patch("scripts._lib.HOME", install_root):
                install(module_file, dry_run=False)

        install_link = install_dir / "rc"
        backup_path = install_dir / ".rc.backup"
        assert install_link.is_symlink()
        assert install_link.resolve() == module_file.resolve()
        assert backup_path.exists()
        assert backup_path.read_text() == "original content"


def test_install_directory_module():
    with tempfile.TemporaryDirectory() as tmp:
        module_root = Path(tmp).resolve() / "modules"
        module_dir = module_root / "mymod"
        module_dir.mkdir(parents=True)
        child_file = module_dir / "rc"
        child_file.touch()
        install_root = Path(tmp).resolve() / "home"
        install_root.mkdir()

        with patch("scripts.install.git_root", return_value=Path(tmp).resolve()):
            with patch("scripts._lib.HOME", install_root):
                install(module_dir, dry_run=False)

        install_link = install_root / ".mymod" / "rc"
        assert install_link.is_symlink()
        assert install_link.resolve() == child_file.resolve()


def test_install_folder_marker_symlinks_directory():
    with tempfile.TemporaryDirectory() as tmp:
        module_root = Path(tmp).resolve() / "modules"
        module_dir = module_root / "mymod"
        module_dir.mkdir(parents=True)
        (module_dir / ".install-folder").touch()
        child_file = module_dir / "rc"
        child_file.touch()
        install_root = Path(tmp).resolve() / "home"
        install_root.mkdir()

        with patch("scripts.install.git_root", return_value=Path(tmp).resolve()):
            with patch("scripts._lib.HOME", install_root):
                install(module_dir, dry_run=False)

        install_link = install_root / ".mymod"
        assert install_link.is_symlink()
        assert install_link.resolve() == module_dir.resolve()
        assert (install_link / "rc").resolve() == child_file.resolve()


def test_install_folder_marker_backs_up_existing_directory():
    with tempfile.TemporaryDirectory() as tmp:
        module_root = Path(tmp).resolve() / "modules"
        module_dir = module_root / "mymod"
        module_dir.mkdir(parents=True)
        (module_dir / ".install-folder").touch()
        install_root = Path(tmp).resolve() / "home"
        install_dir = install_root / ".mymod"
        install_dir.mkdir(parents=True)
        existing_file = install_dir / "rc"
        existing_file.write_text("original content")

        with patch("scripts.install.git_root", return_value=Path(tmp).resolve()):
            with patch("scripts._lib.HOME", install_root):
                install(module_dir, dry_run=False)

        backup_path = install_root / ".mymod.backup"
        install_link = install_root / ".mymod"
        assert install_link.is_symlink()
        assert install_link.resolve() == module_dir.resolve()
        assert backup_path.is_dir()
        assert (backup_path / "rc").read_text() == "original content"


def test_install_dry_run_no_changes():
    with tempfile.TemporaryDirectory() as tmp:
        module_root, module_file = _make_module_tree(tmp)
        install_root = Path(tmp).resolve() / "home"
        install_root.mkdir()

        with patch("scripts.install.git_root", return_value=Path(tmp).resolve()):
            with patch("scripts._lib.HOME", install_root):
                install(module_file, dry_run=True)

        install_link = install_root / ".mymod" / "rc"
        assert not install_link.exists()
