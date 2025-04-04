from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPR2BUILD

bnr = BuilderAndRunner()

def run(cmd):
    print("$ " + " ".join(cmd));
    if cmd[0] == GPR2BUILD:
        out = bnr.check_output(cmd).out
    else:
        out = bnr.simple_run([cmd], catch_error=True).out
    print('\n'.join(sorted(out.splitlines())))

run([GPR2BUILD, "-j1", "-Pprj", "-p", "-j1"])
run([GPR2BUILD, "-j1", "-Pprj", "-p", "-j1"])
run(["./main"])
