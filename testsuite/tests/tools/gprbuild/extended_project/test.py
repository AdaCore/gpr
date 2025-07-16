from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD

bnr = BuilderAndRunner()

def run(cmd):
    print("$ " + " ".join(cmd));
    if cmd[0] == GPRBUILD:
        out = bnr.check_output(cmd).out
    else:
        out = bnr.simple_run([cmd], catch_error=True).out
    print('\n'.join(sorted(out.splitlines())))

run([GPRBUILD, "-p", "-Pprj", "-j1"])
run([GPRBUILD, "-p", "-Pext", "-j1"])
run(["./main"])
run([GPRBUILD, "-p", "-Pext2", "-j1"])
run(["./main"])
