import os
from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPR2BUILD, GPR2CLEAN

bnr = BuilderAndRunner()


def run(cmd):
    print("$ " + " ".join(cmd))
    if cmd[0] in (GPR2BUILD, GPR2CLEAN):
        bnr.call(cmd)
    else:
        print(bnr.simple_run([cmd], catch_error=True).out)


run([GPR2BUILD, "-P", "empty_prj.gpr"])
run([GPR2BUILD, "-P", "empty_prj.gpr", "main"])
run(["./main"])
run([GPR2CLEAN, "-Pempty_prj", "main"])
