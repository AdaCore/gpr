from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD

bnr = BuilderAndRunner()

p = bnr.check_call([GPRBUILD, "-P", "prj.gpr"])
