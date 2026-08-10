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
    """Windows has no runpath: both shared libraries must be on PATH."""
    if "windows" not in Env().host.platform:
        return None

    dirs = [os.path.join("tree", "lib")] + glob.glob(
        os.path.join("tree", "rust", "target", "*", "release")
    )

    return {
        "PATH": os.pathsep.join(
            [os.path.abspath(d) for d in dirs] + [os.environ["PATH"]]
        )
    }


prj = os.path.join("tree", "hello_from_ada.gpr")
bnr.call([GPRBUILD, "-q", "-P", prj, "-p"])
print(
    bnr.simple_run(
        [os.path.join("tree", "hello_from_ada" + exe)],
        env=dll_env(),
        catch_error=True,
    ).out
)
