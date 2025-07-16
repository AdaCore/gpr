import os
from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD, GPRCLEAN

bnr = BuilderAndRunner()


def run(cmd):
    print("$ " + " ".join(cmd))
    if cmd[0] in (GPRBUILD, GPRCLEAN):
        bnr.call(cmd)
    else:
        print(bnr.simple_run([cmd], catch_error=True).out)


run([GPRBUILD, "-P", "empty_prj.gpr"])
run([GPRBUILD, "-P", "empty_prj.gpr", "main"])
run(["./main"])
run([GPRCLEAN, "-Pempty_prj", "main"])
