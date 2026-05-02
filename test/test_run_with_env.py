import json
import sys
from pathlib import Path
from unittest.mock import MagicMock, patch

import importlib.machinery
import importlib.util

BIN_DIR = Path(__file__).parent.parent / "modules" / "local" / "bin"

FAKE_DATA = {
    "env": {"API_KEY": "abc123", "DEBUG": "true"},
    "github": {"token": "gh-secret"},
    "aws": {"access_key": "ak", "secret_key": "sk"},
    "simple": "plain-value",
}


def load_script(name: str):
    path = BIN_DIR / name
    loader = importlib.machinery.SourceFileLoader(name, str(path))
    spec = importlib.util.spec_from_loader(name, loader)
    mod = importlib.util.module_from_spec(spec)
    loader.exec_module(mod)
    return mod


def make_sops_mock(data: dict):
    mock = MagicMock()
    mock.stdout = json.dumps(data).encode()
    return mock


class TestRunWithEnv:
    def setup_method(self):
        self.mod = load_script("run-with-env")

    def test_no_args_exits(self):
        with patch.object(sys, "argv", ["run-with-env"]):
            try:
                self.mod.main()
                assert False, "expected SystemExit"
            except SystemExit as e:
                assert e.code == 1

    def test_missing_enc_file_exits(self, tmp_path):
        with patch.object(sys, "argv", ["run-with-env", "printenv"]):
            with patch.object(self.mod, "ENC_FILE", tmp_path / "missing.enc"):
                try:
                    self.mod.main()
                    assert False, "expected SystemExit"
                except SystemExit as e:
                    assert e.code == 1

    def test_runs_command_with_env(self, tmp_path):
        enc_file = tmp_path / "env.json.enc"
        enc_file.touch()

        with patch.object(sys, "argv", ["run-with-env", "printenv", "API_KEY"]):
            with patch.object(self.mod, "ENC_FILE", enc_file):
                with patch("subprocess.run", return_value=make_sops_mock(FAKE_DATA)):
                    with patch("os.execvpe") as mock_exec:
                        self.mod.main()
                        mock_exec.assert_called_once()
                        cmd, args, env = mock_exec.call_args[0]
                        assert cmd == "printenv"
                        assert args == ["printenv", "API_KEY"]
                        assert env["API_KEY"] == "abc123"
                        assert env["DEBUG"] == "true"

    def test_env_merges_with_os_environ(self, tmp_path):
        enc_file = tmp_path / "env.json.enc"
        enc_file.touch()

        with patch.object(sys, "argv", ["run-with-env", "env"]):
            with patch.object(self.mod, "ENC_FILE", enc_file):
                with patch("subprocess.run", return_value=make_sops_mock(FAKE_DATA)):
                    with patch("os.execvpe") as mock_exec:
                        with patch.dict("os.environ", {"EXISTING": "yes"}):
                            self.mod.main()
                            _, _, env = mock_exec.call_args[0]
                            assert env["EXISTING"] == "yes"
                            assert env["API_KEY"] == "abc123"
