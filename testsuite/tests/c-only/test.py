from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRCLEAN

BuilderAndRunner().call([GPRCLEAN, "prj.gpr"])
