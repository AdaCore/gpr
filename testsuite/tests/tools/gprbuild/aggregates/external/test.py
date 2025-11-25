from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD

bnr = BuilderAndRunner()

bnr.call([GPRBUILD, "-P", "env.gpr"], quiet=True)
p = bnr.check_call([GPRBUILD, "-P", "a.gpr"])
