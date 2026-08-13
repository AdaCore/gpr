from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD, GPRCLEAN
import os

bnr = BuilderAndRunner()

cmd = [GPRBUILD, "-P", "prj.gpr", "-j1"]
print("$ " + " ".join(cmd))
p = bnr.simple_run(cmd, catch_error=False)
print(p.out)

if p.status == 7:
    print("OK: correctly raised a fatal exit code (7)")
else:
    print(f"OK: raised an unexpected exit code ({p.status})")


