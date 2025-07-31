from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRLS, GPRBUILD

bnr = BuilderAndRunner()
bnr.build("test.gpr")

print("Configuration without compiler:")
bnr.call(["./test", "no-compiler.cgpr"])

print("Configuration with C compiler:")
bnr.call(["./test", "c-compiler.cgpr"])

print("Configuration with Ada compiler:")
bnr.call(["./test", "ada-compiler.cgpr"])