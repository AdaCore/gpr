from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRCONFIG
bnr = BuilderAndRunner()
bnr.call ([GPRCONFIG, "--target=linux-fake-target", "--batch", "--config=Ada,,native,"])
bnr.build (project='test.gpr', args=["-q","-p"])
bnr.call (["./main"])