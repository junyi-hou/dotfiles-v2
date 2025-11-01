from pathlib import Path
from ..scripts._lib import get_target_path, get_backup_path, HOME


def test_get_target_path():
    # Setup environment
    nested_path = Path("config/file.txt")
    direct_path = Path("file.txt")
    
    # Test hidden file conversion
    nested_target = get_target_path(nested_path)
    assert nested_target == HOME / ".config/file.txt"

    direct_target = get_target_path(direct_path)
    assert direct_target == HOME / ".file.txt"


def test_get_backup_path():
    # Test hidden file backup
    hidden_file = Path("/home/user/.hidden")
    backup = get_backup_path(hidden_file)
    assert str(backup) == "/home/user/.hidden.backup"
    
    # Test regular file backup
    regular_file = Path("/home/user/regular.txt")
    backup = get_backup_path(regular_file)
    assert str(backup) == "/home/user/.regular.txt.backup"
