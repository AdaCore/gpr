import os

from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD

bnr = BuilderAndRunner()

bnr.check_call([GPRBUILD, "-P", "ie_agg.gpr", "-g1", "-q", "-p", "-bargs", "-Es"])
print(bnr.simple_run([os.path.join(".", "exe", "main")]).out, end="")
