import os

from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD, GPRINSTALL


def printvars():
    do_print = False
    file = open(os.getcwd() + "/inst/share/gpr/prj.gpr", "r")
    lines = file.readlines()
    file.close()
    for line in lines:
        if do_print:
            print(line[:-1])
        if line.startswith("   for Externally"):
            do_print = True


bnr = BuilderAndRunner()
output = "output.txt"

bnr.run([GPRBUILD, "-p", "-Pprj.gpr"])

prefix_switch = "--prefix=" + os.getcwd() + "/inst"

print("================ 1")

status = bnr.run([GPRINSTALL, prefix_switch, "-f", "-p",
                  "-XPARAM=P1", "-Pprj.gpr"],
                 output=output).status

printvars()

print("================ 2")
status = bnr.run([GPRINSTALL, prefix_switch, "-f", "-p", "--minimal-project",
                  "-XPARAM=P2", "-XPARAM2=P8", "-Pprj.gpr"],
                 output=output).status

printvars()
