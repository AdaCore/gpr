import os

from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRINSTALL

bnr = BuilderAndRunner()

inst_prefix = "--prefix=" + os.getcwd() + "/inst"

def do_inst(v):
    print("======================== " + v)
    prefix = inst_prefix + v;
    output = "output" + v + ".txt"
    prj    = "files/prj" + v + ".gpr"

    status = bnr.run([GPRINSTALL, "-d", "-p",
                      "--target=arm-linux-gnueabihf",
                      "--cross-install",
                      prefix, prj],
                     output=output).status

    file = open(output, "r")
    lines = file.readlines()
    file.close()

    for line in sorted(lines):
        if line.startswith("cp ") or line.startswith('ln'):
            print(line)

do_inst("1")
do_inst("2")
