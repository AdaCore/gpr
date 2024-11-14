from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRLS

BuilderAndRunner().check_output([GPRLS, "-Pbug"])
