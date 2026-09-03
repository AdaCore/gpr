import glob
import os
from e3.env import Env
from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD

bnr = BuilderAndRunner()

if "windows" in Env().host.platform:
    exe = ".exe"
else:
    exe = ""


#  windows has no runpath: the DLL of the shared Ada library must be on PATH.
if "windows" in Env().host.platform:
    env = {
        "PATH": os.path.abspath(os.path.join("tree", "lib"))
        + os.pathsep
        + os.environ["PATH"]
    }
else:
    env = None


def run(cmd):
    if cmd[0] == GPRBUILD:
        bnr.call(cmd)
    else:
        print(bnr.simple_run([cmd], env=env, catch_error=True).out)


run([GPRBUILD, "-q", "-P", os.path.join("tree", "main.gpr"), "-p"])
matches = glob.glob(
    os.path.join("tree", "rust", "target", "*", "release", "main_from_rust" + exe)
)
run([matches[0]])
