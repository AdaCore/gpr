import os

from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD, GPRINSTALL

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

######################################################## 1

bnr = BuilderAndRunner()
output = "output.txt"

prefix_switch = "--prefix=" + os.getcwd() + "/inst"

status = bnr.run([GPRINSTALL, prefix_switch + "1", "-p",
                  "--sources-only",
                  "prj.gpr"],
                 output=output).status

print("=== install 1")
file = open(output)
print(file.readlines())

dir = Path("inst1")
print_tree(dir)

######################################################## 2

status = bnr.run([GPRBUILD, "-p", "-q", "prj.gpr"])

status = bnr.run([GPRINSTALL, prefix_switch + "2", "-p",
                  "--sources-only",
                  "prj.gpr"],
                 output=output).status

print("=== install 2")

file = open(output)
print(file.readlines())

dir = Path("inst2")
print_tree(dir)
