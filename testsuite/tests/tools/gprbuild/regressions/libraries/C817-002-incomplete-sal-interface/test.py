from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD, GPRCLEAN

bnr = BuilderAndRunner()

def run(cmd):
    print("$ " + " ".join(cmd))
    if cmd[0] in (GPRBUILD, GPRCLEAN):
        bnr.call(cmd)
    else:
        print(bnr.simple_run([cmd], catch_error=True).out)

run([GPRBUILD, "-P", "prj.gpr", "-q"])
run([GPRCLEAN, "-P", "prj.gpr", "-q", "-r"])

import os
print("$ lib/ contents after clean")
print(sorted(os.listdir("lib")) if os.path.isdir("lib") else "<removed>")
