import os
from e3.fs import rm
from testsuite_support.builder_and_runner import BuilderAndRunner

bnr = BuilderAndRunner()

def run(cmd):
    if isinstance (cmd, str):
        print("$ " + cmd);
        args = cmd.split(" ")
    else:
        print("$ " + " ".join(cmd));
        args = cmd
    if args[0].startswith("gpr2"):
        bnr.call(args)
    else:
        print(bnr.simple_run(args, catch_error=True).out)

run("gpr2build -Pprj1 -q --no-indirect-imports")
run("gpr2build -Pprj1 -q")
run("./p")
run("gpr2clean -Pprj1 -q")
