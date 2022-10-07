from testsuite_support.builder_and_runner import BuilderAndRunner, GPRCLEAN
import os

bnr = BuilderAndRunner()

bnr.run(["gprbuild", "-p", "-q", "-P", "prj.gpr", "main.adb"])
os.system('rm -rf obj')
bnr.call([GPRCLEAN, "-f", "-P", "prj.gpr", "main"])

bnr.run(["gprbuild", "-p", "-q", "-P", "prj.gpr", "main.adb"])
os.system('rm -rf exec')
bnr.call([GPRCLEAN, "-f", "-P", "prj.gpr", "main"])

bnr.run(["gprbuild", "-p", "-q", "-P", "prj2.gpr"])
os.system('rm -rf lib')
bnr.call([GPRCLEAN, "-f", "-P", "prj2.gpr"])
