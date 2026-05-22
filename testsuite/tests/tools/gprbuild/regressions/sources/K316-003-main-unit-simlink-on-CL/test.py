import os
from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD, GPRCLEAN

from pathlib import Path

bnr = BuilderAndRunner()

def run(cmd):
    print("$ " + " ".join(cmd))
    if cmd[0] in (GPRBUILD, GPRCLEAN):
        bnr.call(cmd)
    else:
        print(bnr.simple_run([cmd], catch_error=True).out)

Path("xsrc").symlink_to("src")
run([GPRBUILD, "-c", "-u", "-P", "proj1.gpr", "xsrc/toto.adb"])
os.unlink("xsrc")
