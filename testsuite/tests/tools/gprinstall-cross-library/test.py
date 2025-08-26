import os

from testsuite_support.builder_and_runner import BuilderAndRunner, GPRINSTALL

bnr = BuilderAndRunner()

inst_prefix = "--prefix=" + os.getcwd() + "/inst"
output = "output.txt"

status = bnr.run([GPRINSTALL, "-d", "-p", "--target=arm-linux-gnueabihf",
                  inst_prefix, "files/prj.gpr"],
                 output=output).status

file = open(output, "r")
lines = file.readlines()
file.close()

for line in lines:
    if line.startswith("cp ") or line.startswith('ln'):
        print(line)
