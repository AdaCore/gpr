from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD

import os

bnr = BuilderAndRunner()

def run(cmd):
    print("$ " + " ".join(cmd))
    if cmd[0] in (GPRBUILD):
        bnr.call(cmd)
    else:
        print(bnr.simple_run(cmd, catch_error=True).out)

def ls_obj():
    return sorted(f for f in os.listdir("obj") if not f.startswith("."))

run([GPRBUILD, "-c", "-P", "prj.gpr", "-u", "main.adb", "main-test_proc.adb", "-gnatc"])
print("$ ls obj")
print(ls_obj())
