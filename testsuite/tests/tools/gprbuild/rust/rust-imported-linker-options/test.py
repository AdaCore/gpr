import glob
import os

from e3.env import Env
from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD

bnr = BuilderAndRunner()

bnr.simple_run([
    GPRBUILD, "-q", "-P", os.path.join("tree", "linker_option_provider.gpr"),
    "-p",
], catch_error=True)
bnr.run([
    GPRBUILD, "-q", "-P", os.path.join("tree", "main.gpr"), "-p",
], catch_error=True)

exe = ".exe" if "windows" in Env().host.platform else ""
matches = glob.glob(os.path.join(
    "tree", "rust", "target", "*", "release", "main_from_rust" + exe
))
assert len(matches) == 1, matches
result = bnr.simple_run([matches[0]], catch_error=True)
print("result:", result.out.strip())
