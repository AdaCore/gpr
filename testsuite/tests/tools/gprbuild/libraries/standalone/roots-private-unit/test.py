import os

from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD

bnr = BuilderAndRunner()
bnr.call([GPRBUILD, "-Pprj.gpr", "-p", "-q"])

# Only the interface unit may be published: Priv is reachable as an entry
# point through Roots, but it is not part of Library_Interface, so neither
# its ALI nor its sources may be copied out of the object directory.

for d in ("lib", "libsrc"):
    print(f"{d}:")
    for f in sorted(os.listdir(d)):
        print(f"  {f}")
