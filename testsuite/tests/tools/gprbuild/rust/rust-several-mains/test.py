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

bnr.call([GPRBUILD, "-q", "-P", os.path.join("tree", "tools.gpr"), "-p"])

#  Both mains were asked for, so Cargo builds and GPR2 tracks both.
for name in ("first", "second"):
    matches = glob.glob(
        os.path.join("tree", "rust", "target", "*", "release", name + exe)
    )
    print(bnr.simple_run([matches[0]], catch_error=True).out)
