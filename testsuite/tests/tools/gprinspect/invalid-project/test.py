from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRINSPECT

bnr = BuilderAndRunner()

p = bnr.run([GPRINSPECT, "-Pprj1.gpr"])
print (f"{p.out}")
