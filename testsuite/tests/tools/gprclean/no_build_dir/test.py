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

#  No deliberate deletion this time: prj.gpr has no declared Main attribute
#  (main.adb is only ever given on the command line), so Remove_Empty_Dirs
#  (-p) needs the command-line main to know Exec_Dir applies to this view.
bnr.run([GPRBUILD, "-p", "-q", "-P", "prj.gpr", "main.adb"])
bnr.call([GPRCLEAN, "-f", "-p", "-P", "prj.gpr", "main"])
print("obj exists:", os.path.isdir("obj"))
print("exec exists:", os.path.isdir("exec"))
