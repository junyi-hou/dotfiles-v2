import sys
import shutil
from subprocess import run

from scripts._lib import logger


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
    return _try_install("@anthropic-ai/claude-code@2.1.126", "claude") and _try_install(
        "@agentclientprotocol/claude-agent-acp@0.30.0", "claude-agent-acp"
    )


if __name__ == "__main__":
    if not _install_claude():
        logger.error("Claude CLI installation failed and is required to proceed.")
        sys.exit(1)
    logger.info("Claude successfully installed!")
