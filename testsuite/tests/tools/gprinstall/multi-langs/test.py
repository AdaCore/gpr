import os

from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD, GPRINSTALL

def printprj():
    file = open(os.getcwd() + "/inst/share/gpr/prj.gpr", "r")
    lines = file.readlines()
    file.close()
    outactive=False
    for line in lines:
        if line.startswith("library "):
            outactive=True
        if outactive:
            print(line[:-1])

bnr = BuilderAndRunner()
output = "output.txt"

bnr.run([GPRBUILD, "-p", "-XLIBRARY_TYPE=static", "prj.gpr"], output=output)
bnr.run([GPRBUILD, "-p", "-XLIBRARY_TYPE=relocatable", "prj.gpr"], output=output)

prefix_switch = "--prefix=" + os.getcwd() + "/inst"

status = bnr.run([GPRINSTALL, prefix_switch, "-p",
                  "-XLIBRARY_TYPE=static", "--build-name=static",
                  "--build-var=LIBRARY_TYPE",
                  "prj.gpr"],
                 output=output).status

status = bnr.run([GPRINSTALL, prefix_switch, "-p",
                  "-XLIBRARY_TYPE=relocatable", "--build-name=relocatable",
                  "--build-var=LIBRARY_TYPE",
                  "prj.gpr"],
                 output=output).status

printprj()
