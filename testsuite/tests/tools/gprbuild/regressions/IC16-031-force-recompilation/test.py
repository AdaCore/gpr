from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD, GPRCLEAN

import os
import stat

bnr = BuilderAndRunner()

def run(cmd):
    print("$ " + " ".join(cmd))
    if cmd[0] in (GPRBUILD, GPRCLEAN):
        bnr.call(cmd)
    else:
        print(bnr.simple_run([cmd], catch_error=True).out)

def chmod_remove_write(path):
    print(f"$ chmod -w {path}")
    current = stat.S_IMODE(os.stat(path).st_mode)
    os.chmod(path, current & ~0o222)

run([GPRBUILD, "-f", "-u", "-P", "prj.gpr"])
run([GPRBUILD, "-f", "-u", "-P", "prj.gpr", "pkg.ads"])
run([GPRBUILD, "-f", "-P", "prj2.gpr"])
chmod_remove_write("main.ali")
run([GPRBUILD, "-f", "-u", "-P", "prj2.gpr"])
run([GPRBUILD, "-f", "-u", "-P", "prj2.gpr", "main.adb"])

run([GPRCLEAN, "-r", "-P", "prj.gpr"])
run([GPRCLEAN, "-r", "-P", "prj2.gpr"])
