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

run([GPRBUILD, "-q", "-P", os.path.join("original", "os.gpr"), "-p"])
run([GPRBUILD, "-q", "-P", os.path.join("derived", "derived.gpr"), "-p", "-cargs", "-gnatws"])
