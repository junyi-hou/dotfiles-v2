import subprocess
from pathlib import Path

import pytest


REPO_ROOT = Path(__file__).resolve().parents[1]
TEMPLATE_CASES = (
    ("pixi", ".tools/bin"),
    ("uv", ".tools"),
    ("scala", ".tools"),
    ("clojure", ".tools"),
)


def _run(cmd: list[str], cwd: Path) -> None:
    subprocess.run(cmd, cwd=cwd, check=True, capture_output=True, text=True)


def _init_git_repo(repo: Path) -> None:
    _run(["git", "init", "-q"], repo)
    _run(["git", "config", "user.email", "test@example.com"], repo)
    _run(["git", "config", "user.name", "Test"], repo)
    (repo / "README.md").write_text("root\n")
    _run(["git", "add", "README.md"], repo)
    _run(["git", "commit", "-q", "-m", "init"], repo)


def _prepare_project(project: Path, template_name: str) -> None:
    project.mkdir(parents=True, exist_ok=True)
    (project / ".envrc").write_text(
        (REPO_ROOT / "direnv_templates" / template_name / "envrc").read_text()
    )
    (project / "pixi.toml").write_text("[workspace]\nchannels = []\nplatforms = []\n")

    tools = project / ".tools"
    for path in (
        tools / "bin",
        tools / "clojure" / "bin",
        tools / "coursier" / "bin",
        tools / "jupyter" / "kernels" / "clojure",
        tools / "venv" / "bin",
        project / ".venv" / "bin",
    ):
        path.mkdir(parents=True, exist_ok=True)

    (tools / "bin" / "uv").touch()
    (tools / "venv" / "bin" / "activate").write_text("")
    (project / ".venv" / "bin" / "activate").write_text("")


def _source_envrc(project: Path, log_file: Path) -> dict[str, str]:
    script = r"""
set -euo pipefail

LOG="$1"

pixi() {
    echo "pixi $*" >> "$LOG"
    if [ "$1" = "shell-hook" ]; then
        echo "true"
    fi
}

uv() {
    echo "uv $*" >> "$LOG"
    if [ "$1" = "venv" ]; then
        mkdir -p "$2/bin"
        : > "$2/bin/activate"
    fi
}

cs() {
    echo "cs $*" >> "$LOG"
}

clojure-lsp() {
    echo "clojure-lsp $*" >> "$LOG"
}

curl() {
    echo "curl $*" >> "$LOG"
}

sh() {
    echo "sh $*" >> "$LOG"
}

source ./.envrc
printf 'PROJECT_ROOT=%s\n' "$_PROJECT_ROOT"
printf 'TOOLS=%s\n' "$_TOOLS"
if [ -n "${_VENV:-}" ]; then
    printf 'VENV=%s\n' "$_VENV"
fi
printf 'PATH_HEAD=%s\n' "${PATH%%:*}"
"""
    result = subprocess.run(
        ["bash", "-c", script, "_", str(log_file)],
        cwd=project,
        check=True,
        capture_output=True,
        text=True,
    )
    return dict(line.split("=", 1) for line in result.stdout.strip().splitlines())


@pytest.mark.parametrize(("template_name", "tools_relative_path"), TEMPLATE_CASES)
def test_envrc_template_uses_project_directory_inside_git_repo(
    tmp_path: Path, template_name: str, tools_relative_path: str
) -> None:
    repo = tmp_path / "repo"
    repo.mkdir()
    _init_git_repo(repo)

    project = repo / "subproject"
    _prepare_project(project, template_name)
    _run(["git", "add", "subproject"], repo)
    _run(["git", "commit", "-q", "-m", "add subproject"], repo)

    values = _source_envrc(project, tmp_path / f"{template_name}.log")

    expected_project = project.resolve()
    assert values["PROJECT_ROOT"] == str(expected_project)
    assert values["TOOLS"] == str(expected_project / tools_relative_path)
    assert values["PATH_HEAD"] == str(expected_project / ".tools" / "bin")
    assert not (repo / ".tools").exists()
    assert not (repo / ".venv").exists()

    if template_name == "uv":
        assert values["VENV"] == str(expected_project / ".venv")


@pytest.mark.parametrize(("template_name", "tools_relative_path"), TEMPLATE_CASES)
def test_envrc_template_reuses_main_project_directory_from_linked_worktree(
    tmp_path: Path, template_name: str, tools_relative_path: str
) -> None:
    main = tmp_path / "main"
    linked = tmp_path / "linked"
    main.mkdir()
    _init_git_repo(main)

    main_project = main / "subproject"
    _prepare_project(main_project, template_name)
    _run(["git", "add", "subproject"], main)
    _run(["git", "commit", "-q", "-m", "add subproject"], main)
    _run(["git", "worktree", "add", "-q", "--detach", str(linked), "HEAD"], main)

    linked_project = linked / "subproject"
    values = _source_envrc(linked_project, tmp_path / f"{template_name}.log")

    expected_project = main_project.resolve()
    assert values["PROJECT_ROOT"] == str(expected_project)
    assert values["TOOLS"] == str(expected_project / tools_relative_path)
    assert values["PATH_HEAD"] == str(expected_project / ".tools" / "bin")
    assert not (main / ".tools").exists()
    assert not (main / ".venv").exists()
    assert not (linked / ".tools").exists()
    assert not (linked / ".venv").exists()

    if template_name == "pixi":
        log = (tmp_path / f"{template_name}.log").read_text()
        assert f"pixi install --manifest-path {expected_project / 'pixi.toml'}" in log
        assert f"pixi shell-hook --manifest-path {expected_project / 'pixi.toml'}" in log

    if template_name == "uv":
        assert values["VENV"] == str(expected_project / ".venv")
