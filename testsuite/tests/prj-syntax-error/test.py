import os
import subprocess

from testsuite_support.builder_and_runner import BuilderAndRunner, GPRBUILD, GPRLS

bnr = BuilderAndRunner()

# GPRbuild
bnr.run([GPRBUILD, "-p", "-q", "prj.gpr"])

# GPRls
bnr.run([GPRLS, "prj.gpr"])
