import os

from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD

bnr = BuilderAndRunner()
bnr.call([GPRBUILD, "-Plib.gpr", "-p", "-q"])

# A regular library has no interface: every unit it owns must have its ALI
# copied to Library_ALI_Dir.

print("ali:")
for f in sorted(os.listdir("ali")):
    print(f"  {f}")
