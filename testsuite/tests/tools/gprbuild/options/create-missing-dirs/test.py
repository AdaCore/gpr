import os.path

from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD

bnr = BuilderAndRunner()

print("Automatic for simple projects:")
bnr.call([GPRBUILD, "-P", os.path.join("tree", "simple.gpr"), "-j1"])

print("Without -p:")
bnr.call([GPRBUILD, "-P", os.path.join("tree", "main.gpr"), "-j1"])

print("With -p:")
bnr.call([GPRBUILD, "-P", os.path.join("tree", "main.gpr"), "-p", "-j1"])
