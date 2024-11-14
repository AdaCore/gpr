from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRCLEAN, GPRBUILD

import os

bnr = BuilderAndRunner()

bnr.run([GPRBUILD, "-p", "-q", "-P", "prj.gpr", "main.adb"])
os.system('rm -rf obj')
bnr.call([GPRCLEAN, "-f", "-P", "prj.gpr", "main"])

bnr.run([GPRBUILD, "-p", "-q", "-P", "prj.gpr", "main.adb"])
os.system('rm -rf exec')
bnr.call([GPRCLEAN, "-f", "-P", "prj.gpr", "main"])

bnr.run([GPRBUILD, "-p", "-q", "-P", "prj2.gpr"])
os.system('rm -rf lib')
bnr.call([GPRCLEAN, "-f", "-P", "prj2.gpr"])
