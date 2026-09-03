import os
from e3.env import Env
from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD

bnr = BuilderAndRunner()

if "windows" in Env().host.platform:
    exe = ".exe"
else:
    exe = ""


def run(cmd):
    if cmd[0] == GPRBUILD:
        bnr.call(cmd)
    else:
        print(bnr.simple_run([cmd], catch_error=True).out)


run([GPRBUILD, "-q", "-P", os.path.join("tree", "hello_from_ada.gpr"), "-p"])
run([os.path.join("tree", "hello_from_ada" + exe)])
