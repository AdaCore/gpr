from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD
from e3.env import Env

bnr = BuilderAndRunner()

if "windows" in Env().host.platform:
    exe=".exe"
else:
    exe=""

def run(cmd):
    print("$ " + " ".join(cmd))
    if cmd[0] in (GPRBUILD):
        bnr.call(cmd)
    else:
        print(bnr.simple_run([cmd], catch_error=True).out)

run([GPRBUILD, "-p", "-P", "tree/p.gpr", "-j1", "--src-subdirs=subdir"])
run([f"./tree/obj/main{exe}"])
run([GPRBUILD, "-p", "-P", "tree2/p.gpr", "-j1", "--src-subdirs=subdir"])
