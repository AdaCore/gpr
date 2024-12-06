from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRLS, GPRBUILD

bnr = BuilderAndRunner()

bnr.call([GPRBUILD, "-q", "-p", "-P", "demo.gpr"])
bnr.call([GPRLS, "-P", "demo.gpr"])