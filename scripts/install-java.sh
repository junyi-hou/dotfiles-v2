set -euo pipefail

_PROJECT_ROOT=$(git rev-parse --show-toplevel)
_TOOLS="${_PROJECT_ROOT}/.tools"

if [ -d "${_TOOLS}/java" ]; then
    echo "java already installed at ${_TOOLS}/java"
    exit 0
fi

## supports {darwin, linux}x{aarch64, x86_64}
__UNAME_S="$(uname -s)"
case "${__UNAME_S}" in
    Darwin) _OS="macos" ;;
    Linux)  _OS="linux" ;;
    *)
        echo "Unsupported OS: ${__UNAME_S}" >&2
        exit 1
        ;;
esac

__UNAME_M="$(uname -m)"
case "${__UNAME_M}" in
    aarch64|arm64) _ARCH="aarch64" ;;
    x86_64|amd64)  _ARCH="x64" ;;
    *)
        echo "Unsupported arch: ${__UNAME_M}" >&2
        exit 1
        ;;
esac

JDK_URL="https://download.oracle.com/graalvm/21/archive/graalvm-jdk-21_${_OS}-${_ARCH}_bin.tar.gz"
TMP_TAR="/tmp/jdk.tar.gz"
curl -fsSL "${JDK_URL}" -o "${TMP_TAR}"

# extract to a temporary directory, then move only the 'Content/Home' directory
TMP_DIR="$(mktemp -d)" || (rm -f "${TMP_TAR}" && exit 1)
tar -xzf "${TMP_TAR}" -C "${TMP_DIR}"

# locate the extracted top-level directory (e.g. graalvm-jdk-21+35.1)
EXTRACTED_ROOT="$(find "${TMP_DIR}" -mindepth 1 -maxdepth 1 -type d | head -n 1)"
if [ -z "${EXTRACTED_ROOT}" ]; then
    echo "Failed to find extracted JDK directory" >&2
    rm -rf "${TMP_DIR}" "${TMP_TAR}"
    exit 1
fi

CONTENT_HOME="${EXTRACTED_ROOT}/Contents/Home"
if [ ! -d "${CONTENT_HOME}" ]; then
    echo "Expected Content/Home not found in archive" >&2
    rm -rf "${TMP_DIR}" "${TMP_TAR}"
    exit 1
fi

# Move only the Home directory into ${_TOOLS}/java
mv "${CONTENT_HOME}" "${_TOOLS}/java/"

# cleanup
rm -rf "${TMP_DIR}"
rm -f "${TMP_TAR}"

echo "Java Graalvm 21 installed"
