import os
from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD

bnr = BuilderAndRunner()

def run(cmd):
    print("$ " + " ".join(cmd))
    if cmd[0] == GPRBUILD:
        bnr.call(cmd)
    else:
        print(bnr.simple_run([cmd], catch_error=True).out)

run([GPRBUILD, "-q", "-P", os.path.join("tree", "app.gpr"), "-p"])
run(["./tree/main.first"])
run(["./tree/main.second"])
