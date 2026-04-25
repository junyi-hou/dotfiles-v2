import pytest
from unittest.mock import patch, MagicMock
from scripts.install_claude import _check_claude, install_plugins


def test_decorator_already_installed():
    """Test that decorator doesn't trigger install if claude exists."""
    mock_func = MagicMock(return_value="success")
    decorated = _check_claude(mock_func)

    with patch("scripts.install_claude.run") as mock_run:
        mock_run.return_value.returncode = 0
        result = decorated("arg1")

        assert result == "success"
        mock_run.assert_called_once()
        # Verify it checked --help
        assert mock_run.call_args[0][0] == ["claude", "--help"]


@patch("scripts.install_claude._run_claude")
@patch("scripts.install_claude.logger")
def test_install_plugins(mock_logger, mock_run_claude):
    """Test that it installs all plugins."""
    install_plugins()

    assert mock_run_claude.call_count == 3
    mock_run_claude.assert_any_call(["plugins", "install", "skill-creator@claude-plugins-official"])
    mock_run_claude.assert_any_call(["plugins", "install", "code-review@claude-plugins-official"])
    mock_run_claude.assert_any_call(["plugins", "install", "context7@claude-plugins-official"])
