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

run([GPRBUILD, "-P", "ada_test_a/hello_a.gpr", "-f"])
run([GPRBUILD, "-P", "develop_a/user_hello_a.gpr", "-f"])
run([GPRBUILD, "-P", "a/b/c/c.gpr", "-f"])
run([GPRBUILD, "-P", "a/b/b.gpr", "-f"])
run([GPRBUILD, "-P", "a/a.gpr", "-f"])
