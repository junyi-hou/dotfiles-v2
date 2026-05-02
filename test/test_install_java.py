import os
import tempfile
import pytest
from unittest.mock import patch
from scripts.install_java import get_os_and_arch, get_jdk_url, extract_and_install


def test_get_os_and_arch_darwin():
    os_name, arch = get_os_and_arch(system="Darwin", machine="arm64")
    assert os_name == "macos"
    assert arch == "aarch64"


def test_get_os_and_arch_linux():
    os_name, arch = get_os_and_arch(system="Linux", machine="x86_64")
    assert os_name == "linux"
    assert arch == "x64"


def test_get_os_and_arch_unsupported():
    with pytest.raises(ValueError):
        get_os_and_arch(system="Windows", machine="x86_64")


def test_get_jdk_url():
    url = get_jdk_url("macos", "aarch64")
    assert "graalvm-jdk-21_macos-aarch64_bin.tar.gz" in url


def test_extract_and_install_linux():
    with tempfile.TemporaryDirectory() as tmp_dir:
        jdk_root = os.path.join(tmp_dir, "jdk-21")
        os.makedirs(jdk_root)
        with open(os.path.join(jdk_root, "release"), "w") as f:
            f.write("release info")

        target_java_dir = os.path.join(tmp_dir, "target_java")

        with patch("tarfile.open"):
            extract_and_install("mock.tar.gz", tmp_dir, target_java_dir, system="Linux")

        assert os.path.exists(target_java_dir)
        assert os.path.exists(os.path.join(target_java_dir, "release"))


def test_extract_and_install_macos():
    with tempfile.TemporaryDirectory() as tmp_dir:
        jdk_root = os.path.join(tmp_dir, "jdk-21")
        content_home = os.path.join(jdk_root, "Contents", "Home")
        os.makedirs(content_home)
        with open(os.path.join(content_home, "release"), "w") as f:
            f.write("release info")

        target_java_dir = os.path.join(tmp_dir, "target_java_macos")

        with patch("tarfile.open"):
            extract_and_install(
                "mock.tar.gz", tmp_dir, target_java_dir, system="Darwin"
            )

        assert os.path.exists(target_java_dir)
        assert os.path.exists(os.path.join(target_java_dir, "release"))
