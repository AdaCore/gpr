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

#  Never from inside tree/: Cargo.Root is relative, and the directory it is
#  relative to is the project file's, not this one.
bnr.call([GPRBUILD, "-q", "-P", prj, "-p"])

matches = glob.glob(artifact)
print("built:", bool(matches))

if matches:
    print(bnr.simple_run([matches[0]], catch_error=True).out)

bnr.check_output([GPRCLEAN, "-P", prj])
print("cleaned:", not glob.glob(artifact))
