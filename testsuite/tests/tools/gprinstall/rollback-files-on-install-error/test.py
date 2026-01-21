import os

from testsuite_support.builder_and_runner import BuilderAndRunner, GPRINSTALL

bnr = BuilderAndRunner()

prefix_switch = "--prefix=" + os.getcwd() + "/inst"
print(bnr.run([GPRINSTALL, "-p", prefix_switch, "-f", "-P", "install.gpr"],
              output=None).status)
