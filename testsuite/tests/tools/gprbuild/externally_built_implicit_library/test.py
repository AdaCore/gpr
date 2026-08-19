from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD

bnr = BuilderAndRunner()


def run(cmd):
    print("$ " + " ".join(cmd))
    if cmd[0] == GPRBUILD:
        bnr.call(cmd)
    else:
        print(bnr.simple_run([cmd], catch_error=True).out)


run([GPRBUILD, "-q", "-Pprovider/provider.gpr", "-p"])
run([GPRBUILD, "-q", "-Pmain.gpr", "-p"])
run(["./main"])
