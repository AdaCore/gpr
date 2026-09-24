import os

from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD

bnr = BuilderAndRunner()
bnr.call([GPRBUILD, "-Plib.gpr", "-p", "-q"])

# Specs are always published. A body is published only when the client needs
# it to compile: Gen is generic, and Bodyonly has no spec at all. Plain's
# body stays private.

for d in ("ali", "libsrc"):
    print(f"{d}:")
    for f in sorted(os.listdir(d)):
        print(f"  {f}")
