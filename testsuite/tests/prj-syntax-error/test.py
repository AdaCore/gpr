import os
import subprocess

from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD

bnr = BuilderAndRunner()

# GPRbuild
bnr.run([GPRBUILD, "-p", "-q", "prj.gpr"], output=True)
