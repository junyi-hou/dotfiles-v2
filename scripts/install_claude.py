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
        run("curl -fsSL https://claude.ai/install.sh | bash", shell=True, check=True)
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


def get_list_of_installed_plugins() -> list[str]:
    """
    Parses the output of `claude plugins list` to return a list of plugin names.
    """
    output = _run_claude(["plugins", "list"])
    plugins = []
    for line in output.splitlines():
        line = line.strip()
        # Look for lines starting with the selection indicator or just the plugin name
        if line.startswith("❯"):
            plugin_name = line.lstrip("❯").strip()
            plugins.append(plugin_name)
        elif (
            "@" in line
            and not line.startswith("Version:")
            and not line.startswith("Scope:")
            and not line.startswith("Status:")
        ):
            # Fallback for lines like 'skill-creator@claude-plugins-official' without indicator
            plugin_name = line.strip()
            if plugin_name and plugin_name not in ["Installed plugins:"]:
                plugins.append(plugin_name)
    return plugins


@_check_claude
def install_missing_plugins() -> None:
    """Install any missing plugins in `LIST_OF_PLUGINS`."""
    installed_plugins = get_list_of_installed_plugins()

    for plugin in LIST_OF_PLUGINS:
        if not plugin or plugin in installed_plugins:
            continue

        try:
            logger.info(f"Installing plugin: {plugin}...")
            _run_claude(["plugins", "install", plugin])
        except Exception as e:
            logger.error(f"Failed to install plugin {plugin}: {e}")
            sys.exit(1)


if __name__ == "__main__":
    install_missing_plugins()
    logger.info("Claude successfully installed!")
