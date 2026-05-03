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
