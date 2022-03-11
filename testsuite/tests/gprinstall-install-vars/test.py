import os

from testsuite_support.builder_and_runner import BuilderAndRunner, GPRINSTALL


def printvars():
    file = open(os.getcwd() + "/inst/share/gpr/prj.gpr", "r")
    lines = file.readlines()
    file.close()
    for line in lines:
        if line.startswith("   Para"):
            print(line[:-1])


bnr = BuilderAndRunner()
output = "output.txt"

bnr.run(["gprbuild", "-p", "-Pprj.gpr"])

prefix_switch = "--prefix=" + os.getcwd() + "/inst"

print("================ 1")

status = bnr.run([GPRINSTALL, prefix_switch, "-f", "-p",
                  "-XPARAM=P1", "-Pprj.gpr"],
                 output=output).status

printvars()

print("================ 2")
status = bnr.run([GPRINSTALL, prefix_switch, "-f", "-p",
                  "-XPARAM=P2", "-XPARAM2=P8", "-Pprj.gpr"],
                 output=output).status

printvars()
