import os
import subprocess

from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRINSTALL, GPRBUILD

from pathlib import Path

def print_tree(path, prefix=''):
    for item in sorted(path.iterdir()):
        if item.is_dir():
            print(f'{prefix}|- {item.name}')
            print_tree(item, prefix + '|   ')
    for item in sorted(path.iterdir()):
        if item.is_dir():
            pass
        elif item.is_symlink():
            print(f'{prefix}|- {item.name} -> {item.readlink()}')
        else:
            print(f'{prefix}|- {item.name}')

bnr = BuilderAndRunner()

# build xyz
bnr.check_output([GPRBUILD, '-p', '-q', '-Pxyz/xyz.gpr'])

# install xyz library
bnr.check_output(
    [GPRINSTALL, '-p', '-r', '--prefix=' + os.getcwd() + '/inst',  'xyz/xyz.gpr'])

dir = Path("inst")
print_tree(dir)
