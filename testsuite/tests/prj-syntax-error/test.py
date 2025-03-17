import os
import subprocess

from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPR2BUILD

bnr = BuilderAndRunner()

# GPRbuild
bnr.run([GPR2BUILD, "-p", "-q", "prj.gpr"], output=True)
