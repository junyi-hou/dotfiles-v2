from subprocess import CalledProcessError
from unittest.mock import patch
from scripts.install_claude import _try_install


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
