#!/usr/bin/env python3
import os
import sys
import platform
import shutil
import subprocess
import tempfile
import urllib.request
import tarfile

def get_project_root():
    try:
        return subprocess.check_output(['git', 'rev-parse', '--show-toplevel'], text=True).strip()
    except subprocess.CalledProcessError:
        return os.getcwd()

def get_os_and_arch(system=None, machine=None):
    system = system or platform.system()
    machine = machine or platform.machine()

    if system == 'Darwin':
        os_name = 'macos'
    elif system == 'Linux':
        os_name = 'linux'
    else:
        raise ValueError(f"Unsupported OS: {system}")

    if machine in ('aarch64', 'arm64'):
        arch = 'aarch64'
    elif machine in ('x86_64', 'amd64'):
        arch = 'x64'
    else:
        raise ValueError(f"Unsupported arch: {machine}")

    return os_name, arch

def get_jdk_url(os_name, arch):
    return f"https://download.oracle.com/graalvm/21/archive/graalvm-jdk-21_{os_name}-{arch}_bin.tar.gz"

def download_file(url, destination):
    print(f"Downloading JDK from {url}...")
    urllib.request.urlretrieve(url, destination)

def extract_and_install(tmp_tar, tmp_dir, java_dir, system=None):
    system = system or platform.system()
    print("Extracting JDK...")
    with tarfile.open(tmp_tar, "r:gz") as tar:
        tar.extractall(path=tmp_dir)

    # Find the extracted top-level directory
    extracted_dirs = [d for d in os.listdir(tmp_dir)
                      if os.path.isdir(os.path.join(tmp_dir, d)) and d != 'java']
    if not extracted_dirs:
        raise RuntimeError("Failed to find extracted JDK directory")

    extracted_root = os.path.join(tmp_dir, extracted_dirs[0])

    if system == 'Darwin':
        content_home = os.path.join(extracted_root, 'Contents', 'Home')
    else:
        content_home = extracted_root

    if not os.path.isdir(content_home):
        raise RuntimeError(f"Expected content home not found at {content_home}")

    os.makedirs(os.path.dirname(java_dir), exist_ok=True)
    if os.path.exists(java_dir):
        shutil.rmtree(java_dir)
    shutil.move(content_home, java_dir)

def main():
    try:
        project_root = get_project_root()
        tools_dir = os.path.join(project_root, '.tools')
        java_dir = os.path.join(tools_dir, 'java')

        if os.path.exists(java_dir):
            print(f"java already installed at {java_dir}")
            return

        os_name, arch = get_os_and_arch()
        jdk_url = get_jdk_url(os_name, arch)

        with tempfile.TemporaryDirectory() as tmp_dir:
            tmp_tar = os.path.join(tmp_dir, "jdk.tar.gz")
            download_file(jdk_url, tmp_tar)
            extract_and_install(tmp_tar, tmp_dir, java_dir)

        print("Java Graalvm 21 installed")
    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)

if __name__ == "__main__":
    main()
