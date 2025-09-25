import subprocess

from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD, GPRCLEAN

bnr = BuilderAndRunner()

bnr.build("multi.gpr", args=['-p'])

bnr.check_output([GPRBUILD, "-p", "-q", "files/multi.gpr"])
print("================")
print("with ALI files:")
bnr.call(["./main"])
print("================")
print("without ALI files:")
bnr.check_output([GPRCLEAN, "-p", "-q", "files/multi.gpr"])
bnr.call(["./main"])
