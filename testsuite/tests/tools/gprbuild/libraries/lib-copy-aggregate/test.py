import os

from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD

bnr = BuilderAndRunner()
bnr.call([GPRBUILD, "-Pagg.gpr", "-p", "-q"])

# The units of an aggregate library belong to the aggregated views, so the
# copy action must reach across them.

print("ali:")
for f in sorted(os.listdir("ali")):
    print(f"  {f}")
