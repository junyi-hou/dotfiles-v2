from subprocess import CalledProcessError
from unittest.mock import patch, MagicMock
from scripts.install_claude import _check_claude, _try_install, install_plugins


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


@patch("scripts.install_claude.shutil.which", return_value=None)
@patch("scripts.install_claude.run")
def test_try_install_calls_run_when_not_found(mock_run, mock_which):
    """run() is called when the program is not on PATH."""
    result = _try_install("some-pkg", "some-prog")
    assert result is True
    mock_run.assert_called_once_with("npm install -g some-pkg", shell=True, check=True)


@patch("scripts.install_claude.shutil.which", return_value="/usr/bin/some-prog")
@patch("scripts.install_claude.run")
def test_try_install_skips_run_when_found(mock_run, mock_which):
    """run() is not called when the program is already on PATH."""
    result = _try_install("some-pkg", "some-prog")
    assert result is True
    mock_run.assert_not_called()


@patch("scripts.install_claude.shutil.which", return_value=None)
@patch("scripts.install_claude.run", side_effect=CalledProcessError(1, "npm"))
def test_try_install_returns_false_on_error(mock_run, mock_which):
    """_try_install returns False when run() raises."""
    result = _try_install("some-pkg", "some-prog")
    assert result is False


@patch("scripts.install_claude._run_claude")
@patch("scripts.install_claude.logger")
def test_install_plugins(mock_logger, mock_run_claude):
    """Test that it installs all plugins."""
    install_plugins()

    assert mock_run_claude.call_count == 3
    mock_run_claude.assert_any_call(
        ["plugins", "install", "skill-creator@claude-plugins-official"]
    )
    mock_run_claude.assert_any_call(
        ["plugins", "install", "code-review@claude-plugins-official"]
    )
    mock_run_claude.assert_any_call(
        ["plugins", "install", "context7@claude-plugins-official"]
    )
