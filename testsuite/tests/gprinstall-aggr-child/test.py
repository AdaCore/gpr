import os

from testsuite_support.builder_and_runner import BuilderAndRunner, GPRINSTALL


def printwiths():
    file = open(os.getcwd() + "/inst/share/gpr/gnatcoll-gprinstall_bug.gpr", "r")
    lines = file.readlines()
    file.close()
    for line in lines:
        if line.startswith("with \"gnatco"):
            print(line[:-1])


bnr = BuilderAndRunner()
output = "output.txt"

bnr.run(["gprbuild", "-p", "-Pgnatcoll-gprinstall_bug.gpr"])

prefix_switch = "--prefix=" + os.getcwd() + "/inst"

status = bnr.run([GPRINSTALL, prefix_switch, "-f", "-p",
                  "-Pgnatcoll-gprinstall_bug.gpr"],
                 output=output).status

printwiths()

bnr.run(["gprbuild", "-p", "-Ptest.gpr"])

if os.path.exists("obj2/tmain"):
    print("OK")
else:
    print("NOK")
