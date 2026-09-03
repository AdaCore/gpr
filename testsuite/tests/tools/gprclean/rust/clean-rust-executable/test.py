import glob
import os

from e3.env import Env
from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD, GPRCLEAN

bnr = BuilderAndRunner()

exe = ".exe" if "windows" in Env().host.platform else ""
prj = os.path.join("tree", "hello_from_rust.gpr")
artifact = os.path.join(
    "tree", "rust", "target", "*", "release", "hello_from_rust" + exe
)


def status():
    return "found" if glob.glob(artifact) else "missing"


# Build the Rust executable through gprbuild: Cargo drops it under
# tree/rust/target/<triple>/release/.
bnr.check_output([GPRBUILD, "-q", "-p", "-P", prj])
print("after build:", status())

# gprclean must run "cargo clean", removing that artifact. This package is
# the only one in its workspace, so there is nothing to isolate and the whole
# target directory goes.
bnr.check_output([GPRCLEAN, "-P", prj])
print("after clean:", status())
