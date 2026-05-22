from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRCLEAN

bnr = BuilderAndRunner()

bnr.call([GPRCLEAN, "-q", "-P", "p.gpr"])
