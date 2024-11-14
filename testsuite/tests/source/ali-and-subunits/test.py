from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRLS, GPRBUILD

bnr = BuilderAndRunner()

bnr.call([GPRBUILD, "-f", "-q", "-k", "-p", "prj1.gpr", "-cargs",
          "-O", "-gnatn"])
bnr.call([GPRLS, "-Pprj1"])
