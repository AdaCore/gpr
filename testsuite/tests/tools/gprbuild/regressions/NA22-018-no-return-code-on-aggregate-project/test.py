from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD, GPRCLEAN

bnr = BuilderAndRunner()

def run(cmd):
    print("$ " + " ".join(cmd))
    if cmd[0] in (GPRBUILD, GPRCLEAN):
        p = bnr.call(cmd)
        if p.status == 0:
            print("$ SUCCESS")
        else:
            print("$ FAILED")
    else:
        print(bnr.simple_run([cmd], catch_error=True).out)

run([GPRBUILD, "-q", "-P", "aggr.gpr"])
run([GPRBUILD, "-q", "-k", "-P", "prj2.gpr"])
run([GPRBUILD, "-q", "-k", "-P", "aggr.gpr"])
