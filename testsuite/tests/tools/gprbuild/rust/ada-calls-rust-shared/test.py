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


def dll_env():
    """Windows has no runpath: the library Cargo built stays in its own
    target directory, so that directory must be on PATH."""
    if "windows" not in Env().host.platform:
        return None

    dirs = glob.glob(os.path.join("tree", "rust", "target", "*", "release"))

    return {
        "PATH": os.pathsep.join(
            [os.path.abspath(d) for d in dirs] + [os.environ["PATH"]]
        )
    }


def run(cmd):
    if cmd[0] == GPRBUILD:
        bnr.call(cmd)
    else:
        print(bnr.simple_run([cmd], env=dll_env(), catch_error=True).out)


run([GPRBUILD, "-q", "-P", os.path.join("tree", "hello_from_ada.gpr"), "-p"])
run([os.path.join("tree", "hello_from_ada" + exe)])
