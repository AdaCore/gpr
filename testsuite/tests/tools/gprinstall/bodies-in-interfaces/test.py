import os
import subprocess

from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRINSTALL, GPRBUILD

from pathlib import Path

def print_prj(name, path):
    print("===== " + name)
    file = open(path, "r")
    lines = file.readlines()
    file.close()

    for line in lines[2:]:
        print(line[:-1])

bnr = BuilderAndRunner()

# build
bnr.check_output([GPRBUILD, '-p', '-q', 'mylib.gpr'])

# install
bnr.check_output(
    [GPRINSTALL, '-p', '--prefix=' + os.getcwd() + '/inst', 'mylib.gpr'])

print_prj("inst", os.getcwd() + "/inst/share/gpr/mylib.gpr")
