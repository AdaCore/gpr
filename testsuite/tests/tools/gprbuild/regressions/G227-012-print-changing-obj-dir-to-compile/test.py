from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD, GPRCLEAN

bnr = BuilderAndRunner()

def run(cmd):
    print("$ " + " ".join(cmd))
    if cmd[0] in (GPRBUILD, GPRCLEAN):
        bnr.call(cmd)
    else:
        print(bnr.simple_run([cmd], catch_error=True).out)

run([GPRBUILD, "-v", "-P", "toto.gpr", "-c", "-u", "proc.adb"]) #previously add "> gprbuild.out 2>&1" to then parse the expected output with sed -n -e "/^Changing to/s#`pwd`#CWD#p" gprbuild.out
