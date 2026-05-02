import sys
import shutil
from functools import wraps
from subprocess import run

from scripts._lib import logger

LIST_OF_PLUGINS = [
    "skill-creator@claude-plugins-official",
    "code-review@claude-plugins-official",
    "context7@claude-plugins-official",
]

LIST_OF_MARKETPLACES = [
    "https://github.com/anthropics/claude-plugins-official.git",
]


def _try_install(pkg: str, prog: str) -> bool:
    if not shutil.which(prog):
        logger.debug(f"Installing {prog}...")
        try:
            run(f"npm install -g {pkg}", shell=True, check=True)
            logger.debug(f"{prog} installed!")
            return True
        except Exception as e:
            logger.error(f"Failed to install Claude CLI: {e}")
            return False

    logger.debug(f"{prog} found, skip install")
    return True


def _install_claude() -> bool:
    return (
        _try_install("@anthropic-ai/claude-code", "claude")
        and _try_install("@agentclientprotocol/claude-agent-acp", "claude-agent-acp")
    )


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


def pull_marketplaces() -> None:
    for marketplace in LIST_OF_MARKETPLACES:
        try:
            logger.info(f"Pulling marketplaces: {marketplace}...")
            _run_claude(["plugins", "marketplace", "add", marketplace])
        except Exception as e:
            logger.error(f"Failed to pull marketplace {marketplace}: {e}")
            sys.exit(1)


def install_plugins() -> None:
    """Install plugins from `LIST_OF_PLUGINS`."""
    for plugin in LIST_OF_PLUGINS:
        try:
            logger.info(f"Installing plugin: {plugin}...")
            _run_claude(["plugins", "install", plugin])
        except Exception as e:
            logger.error(f"Failed to install plugin {plugin}: {e}")
            sys.exit(1)


if __name__ == "__main__":
    pull_marketplaces()
    install_plugins()
    logger.info("Claude successfully installed!")
