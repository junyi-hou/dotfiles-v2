import sys
import shutil
from functools import wraps
from subprocess import run

from scripts._lib import logger

LIST_OF_PLUGINS = [
    "skill-creator@claude-plugins-official",
    "code-review@claude-plugins-official",
]


def _install_claude() -> bool:
    try:
        logger.debug("Installing Claude CLI...")
        run("npm install -g @anthropic-ai/claude-code", shell=True, check=True)
        if not shutil.which("claude-agent-acp"):
            run(
                "npm install -g @agentclientprotocol/claude-agent-acp",
                shell=True,
                check=True,
            )
        return True
    except Exception as e:
        logger.error(f"Failed to install Claude CLI: {e}")
        return False


def _check_claude(func):
    @wraps(func)
    def wrapper(*args, **kwargs):
        try:
            result = run(["claude", "--help"], check=True, capture_output=True)
            if result.returncode == 0:
                return func(*args, **kwargs)
            logger.error("`claude --help` has non-zero return code, please check")
            sys.exit(1)
        except FileNotFoundError:
            if _install_claude():
                return func(*args, **kwargs)
            logger.error("Claude CLI installation failed and is required to proceed.")
            sys.exit(1)

    return wrapper


@_check_claude
def _run_claude(commands: list[str]) -> str:
    result = run(["claude"] + commands, check=True, capture_output=True, text=True)
    return result.stdout


@_check_claude
def install_plugins() -> None:
    """Install plugins from `LIST_OF_PLUGINS`."""
    for plugin in LIST_OF_PLUGINS:
        if not plugin:
            continue

        try:
            logger.info(f"Installing plugin: {plugin}...")
            _run_claude(["plugins", "install", plugin])
        except Exception as e:
            logger.error(f"Failed to install plugin {plugin}: {e}")
            sys.exit(1)


if __name__ == "__main__":
    install_plugins()
    logger.info("Claude successfully installed!")
