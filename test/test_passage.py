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


class TestPassage:
    def setup_method(self):
        self.mod = load_script("passage")

    def test_missing_enc_file_exits(self, tmp_path):
        with patch.object(sys, "argv", ["passage"]):
            with patch.object(self.mod, "ENC_FILE", tmp_path / "missing.enc"):
                try:
                    self.mod.main()
                    assert False, "expected SystemExit"
                except SystemExit as e:
                    assert e.code == 1

    def test_no_args_prints_full_pass_tree(self, tmp_path, capsys):
        enc_file = tmp_path / "env.json.enc"
        enc_file.touch()

        with patch.object(sys, "argv", ["passage"]):
            with patch.object(self.mod, "ENC_FILE", enc_file):
                with patch("subprocess.run", return_value=make_sops_mock(FAKE_DATA)):
                    self.mod.main()
                    out = capsys.readouterr().out
                    assert json.loads(out) == FAKE_DATA

    def test_lookup_leaf_string(self, tmp_path, capsys):
        enc_file = tmp_path / "env.json.enc"
        enc_file.touch()

        with patch.object(sys, "argv", ["passage", "simple"]):
            with patch.object(self.mod, "ENC_FILE", enc_file):
                with patch("subprocess.run", return_value=make_sops_mock(FAKE_DATA)):
                    self.mod.main()
                    assert capsys.readouterr().out.strip() == "plain-value"

    def test_lookup_nested_path(self, tmp_path, capsys):
        enc_file = tmp_path / "env.json.enc"
        enc_file.touch()

        with patch.object(sys, "argv", ["passage", "github/token"]):
            with patch.object(self.mod, "ENC_FILE", enc_file):
                with patch("subprocess.run", return_value=make_sops_mock(FAKE_DATA)):
                    self.mod.main()
                    assert capsys.readouterr().out.strip() == "gh-secret"

    def test_lookup_nested_dict_prints_json(self, tmp_path, capsys):
        enc_file = tmp_path / "env.json.enc"
        enc_file.touch()

        with patch.object(sys, "argv", ["passage", "aws"]):
            with patch.object(self.mod, "ENC_FILE", enc_file):
                with patch("subprocess.run", return_value=make_sops_mock(FAKE_DATA)):
                    self.mod.main()
                    out = capsys.readouterr().out
                    assert json.loads(out) == FAKE_DATA["aws"]

    def test_lookup_missing_key_exits(self, tmp_path):
        enc_file = tmp_path / "env.json.enc"
        enc_file.touch()

        with patch.object(sys, "argv", ["passage", "nonexistent/key"]):
            with patch.object(self.mod, "ENC_FILE", enc_file):
                with patch("subprocess.run", return_value=make_sops_mock(FAKE_DATA)):
                    try:
                        self.mod.main()
                        assert False, "expected SystemExit"
                    except SystemExit as e:
                        assert e.code == 1
