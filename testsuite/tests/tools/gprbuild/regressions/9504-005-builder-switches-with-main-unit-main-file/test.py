from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD, GPRCLEAN

bnr = BuilderAndRunner()

def run(cmd):
    print("$ " + " ".join(cmd))
    if cmd[0] in (GPRBUILD, GPRCLEAN):
        bnr.call(cmd)
    else:
        print(bnr.simple_run([cmd], catch_error=True).out)

run([GPRBUILD, "-f", "-P", "prj.gpr", "main"])
run([GPRBUILD, "-f", "-P", "prj.gpr", "main.2.ada"])
run([GPRBUILD, "-f", "-P", "prj.gpr", "main2"])
run([GPRBUILD, "-f", "-P", "prj.gpr", "main2.2.ada"])
run([GPRBUILD, "-f", "-c", "-P", "prj.gpr", "main3"]) # -q ?
run([GPRBUILD, "-f", "-b", "-P", "prj.gpr", "main3"])
