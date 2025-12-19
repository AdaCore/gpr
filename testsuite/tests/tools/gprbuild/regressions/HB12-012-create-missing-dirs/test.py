import os
from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD, GPRCLEAN

bnr = BuilderAndRunner()

bnr.run([GPRBUILD, "-q", "-P", "proj.gpr", "--create-missing-dirs", "main.adb"])
