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

placeholder = "-p <package named by cargo metadata>"


def status():
    return "found" if glob.glob(artifact) else "missing"


bnr.check_output([GPRBUILD, "-q", "-p", "-P", prj])
print("after build:", status())

#  -n reports the command without running anything. Naming the package is
#  Cargo's answer to give, so a dry run cannot have it: printing the real name
#  here would mean "cargo metadata" had been run.
out = bnr.check_output([GPRCLEAN, "-n", "-P", prj]).out

print("package left unresolved:", placeholder in out)
print("after dry run:", status())

bnr.check_output([GPRCLEAN, "-P", prj])
print("after clean:", status())
