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

# build prj
bnr.check_output([GPRBUILD, '-p', '-q', 'prj.gpr'])

# install prj library
bnr.check_output(
    [GPRINSTALL, '-p', '--prefix=' + os.getcwd() + '/inst1',
     '--gpr-registry-file=gpr-registry.json', 'prj.gpr'])
bnr.check_output(
    [GPRINSTALL, '-p', '--minimal-project', '--prefix=' + os.getcwd() + '/inst2',
     '--gpr-registry-file=gpr-registry.json', 'prj.gpr'])

print_prj("inst1", os.getcwd() + "/inst1/share/gpr/prj.gpr")
print_prj("inst2", os.getcwd() + "/inst2/share/gpr/prj.gpr")
