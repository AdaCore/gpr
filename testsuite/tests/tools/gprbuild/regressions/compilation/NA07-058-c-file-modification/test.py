
from os import remove, utime
from shutil import copy
from time import sleep, time

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

past = time()

sleep(2)
print("first compilation")
copy("toto.c_1", "toto.c")
run([GPRBUILD, "-P", "prj.gpr"])

sleep(2)
print("no changes: no recompilation")
run([GPRBUILD, "-P", "prj.gpr"])

sleep(2)
print("update timestamp (now): no recompilation")
now = time()
utime("toto.c", (now, now))
run([GPRBUILD, "-P", "prj.gpr"])

sleep(2)
print("update timestamp (past): no recompilation")
utime("toto.c", (past, past))
run([GPRBUILD, "-P", "prj.gpr"])

sleep(2)
print("change file and update timestamp (past): recompilation")
copy("toto.c_2", "toto.c")
utime("toto.c", (past, past))
run([GPRBUILD, "-P", "prj.gpr"])

# Cleanup
run([GPRCLEAN, "-P", "prj.gpr"])
remove("toto.c")
