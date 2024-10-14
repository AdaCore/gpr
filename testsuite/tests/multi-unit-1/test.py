import subprocess

from testsuite_support.builder_and_runner import BuilderAndRunner, GPRBUILD

bnr = BuilderAndRunner()

bnr.check_output([GPRBUILD, "-p", "-q", "files/multi.gpr"])
bnr.build("multi.gpr", args=['-p'])
bnr.call(["./main"])
