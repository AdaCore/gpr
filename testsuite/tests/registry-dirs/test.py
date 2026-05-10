import os
import subprocess

from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRINSTALL, GPRBUILD

from pathlib import Path

def print_prj(name):
    print("===== " + name)
    path = os.getcwd() + "/" + name +"/share/gpr/prj.gpr"
    file = open(path, "r")
    lines = file.readlines()
    file.close()

    for line in lines[2:]:
        print(line[:-1])

bnr = BuilderAndRunner()

# build prj
bnr.check_output([GPRBUILD, '-p', '-q', 'prj.gpr'])

# install prj library
bnr.simple_run(
    [GPRINSTALL, '-p', '--prefix=' + os.getcwd() + '/inst1',
     '--gpr-registry-file=gpr-reg/gpr-registry.json', 'prj.gpr'])

bnr.simple_run(
    [GPRINSTALL, '-p', '--prefix=' + os.getcwd() + '/inst2',
     '-XReg=no', 'prj.gpr'])

bnr.simple_run(
    [GPRINSTALL, '-p', '--prefix=' + os.getcwd() + '/inst3',
     '-XReg=yes', 'prj.gpr'])

env = os.environ.copy()
env["GPR_REGISTRY_DIRS"]=os.pathsep.join(["/ici", "", os.getcwd()+"/gpr-reg"])

bnr.simple_run(
    [GPRINSTALL, '-p', '--prefix=' + os.getcwd() + '/inst4',
     '-XReg=no', 'prj.gpr'], env=env)

print_prj("inst1")
print_prj("inst2")
print_prj("inst3")
print_prj("inst4")
