from e3.os.process import Run
from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRLS, GPRBUILD

bnr = BuilderAndRunner()

Run([GPRBUILD, "-p", "-q", "-Pprj"])

p = bnr.check_output([GPRLS, "-o", "-u", "-Pprj"])
print(p.out)
p = bnr.check_output([GPRLS, "-s", "-u", "-Pprj"])
print(p.out)
p = bnr.check_output([GPRLS, "-s", "-o", "-Pprj"])
print(p.out)
