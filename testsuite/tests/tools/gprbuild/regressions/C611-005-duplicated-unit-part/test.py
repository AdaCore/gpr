from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD, GPRCLEAN

bnr = BuilderAndRunner()

def run(cmd):
    print("$ " + " ".join(cmd))
    if cmd[0] in (GPRBUILD, GPRCLEAN):
        bnr.call(cmd)
    else:
        print(bnr.simple_run([cmd], catch_error=True).out)

run([GPRBUILD, "-P", "prj.gpr", "-XPLATFORM=CROSS"])
run(["./main"])
run([GPRBUILD, "-P", "prj.gpr", "-XPLATFORM=NATIVE"])
run(["./main"])
run([GPRBUILD, "-P", "prj2.gpr"])
run([GPRBUILD, "-P", "prj3.gpr", "-XPLATFORM=CROSS"])
run([GPRBUILD, "-P", "prj3.gpr", "-XPLATFORM=NATIVE"])
