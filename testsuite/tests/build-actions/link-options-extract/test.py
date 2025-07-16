from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD
import os

bnr = BuilderAndRunner()

bnr.simple_run([GPRBUILD, "-P" + os.path.join("tree", "lib.gpr"), "-p", "-q"])
bnr.build("test.gpr", args=['-p', '-q'])

bnr.call([os.path.join ("obj", "test")])
