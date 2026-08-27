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


def built(name):
    #  Cargo writes under a target directory of its own for each view, so
    #  either binary is looked for beside its own manifest.
    return bool(
        glob.glob(
            os.path.join(
                "tree", "rust_" + name, "target", "*", "release", name + exe
            )
        )
    )


#  Name a main belonging to First. Second declares a binary of its own, and
#  nothing asked for it.
bnr.call(
    [GPRBUILD, "-q", "-P", os.path.join("tree", "agg.gpr"), "-p", "first.rs"]
)

print("first built:  " + str(built("first")))
print("second built: " + str(built("second")))
