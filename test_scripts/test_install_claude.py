import pytest
from unittest.mock import patch, MagicMock
from scripts.install_claude import _check_claude, get_list_of_installed_plugins, install_missing_plugins


def test_get_list_of_installed_plugins_basic():
    """Test standard plugin listing output."""
    mock_output = """Installed plugins:

  ❯ skill-creator@claude-plugins-official
    Version: 104d39be10b7
    Status: ✔ enabled

    code-review@claude-plugins-official
    Version: abc123def456
    Status: ✔ enabled
"""
    with patch("scripts.install_claude._run_claude", return_value=mock_output):
        plugins = get_list_of_installed_plugins()
        assert "skill-creator@claude-plugins-official" in plugins
        assert "code-review@claude-plugins-official" in plugins
        assert len(plugins) == 2


def test_get_list_of_installed_plugins_empty():
    """Test output with no plugins."""
    mock_output = "No plugins installed."
    with patch("scripts.install_claude._run_claude", return_value=mock_output):
        plugins = get_list_of_installed_plugins()
        assert plugins == []


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
@patch("scripts.install_claude.get_list_of_installed_plugins")
@patch("scripts.install_claude.logger")
def test_install_missing_plugins(mock_logger, mock_get_plugins, mock_run_claude):
    """Test that it installs only missing plugins."""
    # List one as installed, one as missing
    mock_get_plugins.return_value = ["skill-creator@claude-plugins-official"]

    # LIST_OF_PLUGINS has ["skill-creator@...", "code-review@..."]
    install_missing_plugins()

    # code-review should be installed
    mock_run_claude.assert_called_with(["plugins", "install", "code-review@claude-plugins-official"])
    # skill-creator should be skipped
    assert mock_run_claude.call_count == 1
