from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD, GPRCLEAN

bnr = BuilderAndRunner()

def run(cmd):
    print("$ " + " ".join(cmd))
    if cmd[0] in (GPRBUILD, GPRCLEAN):
        bnr.call(cmd)
    else:
        print(bnr.simple_run([cmd], catch_error=True).out)

run([GPRBUILD, "-P", "prj.gpr"])
run([GPRBUILD, "-P", "prj1.gpr", "-q"]) # this should hide the warnings on case construction incomplete-ness
run([GPRBUILD, "-P", "prj1.gpr"])
run([GPRBUILD, "-P", "prj2.gpr"])
run([GPRBUILD, "-P", "prj2.gpr", "-q"]) # this should hide the warnings on case construction incomplete-ness
run([GPRBUILD, "-P", "prj3.gpr"])
