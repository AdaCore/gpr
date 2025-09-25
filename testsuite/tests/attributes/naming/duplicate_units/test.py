from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD

bnr = BuilderAndRunner()
bnr.call([GPRBUILD, "-q", "-Ptest.gpr", "-p"])
