import os
from e3.fs import rm
from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD, GPRCLEAN

bnr = BuilderAndRunner()

def run(cmd):
    if isinstance (cmd, str):
        print("$ " + cmd);
        args = cmd.split(" ")
    else:
        print("$ " + " ".join(cmd));
        args = cmd
    if args[0] in (GPRBUILD, GPRCLEAN):
        bnr.call(args)
    else:
        print(bnr.simple_run(args, catch_error=True).out)

run(f"{GPRBUILD} -Pprj1 -q --no-indirect-imports")
run(f"{GPRBUILD} -Pprj1 -q")
run("./p")
run(f"{GPRCLEAN} -Pprj1 -q")
