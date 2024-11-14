from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD, GPRLS

bnr = BuilderAndRunner()
bnr.check_call([GPRBUILD, "-Pprj", "-q"])
bnr.check_call([GPRLS, "-Pprj", "--closure"])
