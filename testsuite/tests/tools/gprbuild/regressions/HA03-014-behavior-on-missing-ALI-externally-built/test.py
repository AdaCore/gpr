from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD, GPRCLEAN

import os
from pathlib import Path
import stat

bnr = BuilderAndRunner()

def run(cmd):
    print("$ " + " ".join(cmd))
    if cmd[0] in (GPRBUILD, GPRCLEAN):
        bnr.call(cmd)
    else:
        print(bnr.simple_run([cmd], catch_error=True).out)

lib_objs_path = Path("lib_objs")

run([GPRBUILD, "-f", "-q", "-p", "-P", "build_lib.gpr"])
Path("alis/p.ali").rename("alis/aaa.ali")
run([GPRBUILD, "-f", "-q", "-P", "driver.gpr"])
run([GPRBUILD, "-f", "-q", "-p", "-P", "build_lib.gpr"])
for file in lib_objs_path.iterdir():
    if file.is_file():
        file.unlink()
run([GPRBUILD, "-f", "-q", "-P", "driver.gpr"])
