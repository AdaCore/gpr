import os

from testsuite_support.builder_and_runner import BuilderAndRunner, GPRINSTALL

bnr = BuilderAndRunner()
output = "output.txt"

prefix_switch = "--prefix=" + os.getcwd() + "/inst"
status = bnr.run([GPRINSTALL, "-d", "-p", prefix_switch, "prj.gpr"],
                 output=output).status
file = open(output, "r")
lines = file.readlines()
file.close()

for line in lines:
    if line.startswith("cp ") or line.startswith('ln'):
        print(line)
