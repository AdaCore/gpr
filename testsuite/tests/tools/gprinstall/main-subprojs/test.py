import os
from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRINSTALL, GPRBUILD

bnr = BuilderAndRunner()

def check_bin(main):
    prefix='install/bin/'
    if os.path.exists(prefix + main) or os.path.exists(prefix + main + '.exe'):
        print("OK: " + main)
    else:
        print("NOK: "  + main + " not installed")

# For default
bnr.run([GPRBUILD, "-p", "-q", "a2.gpr"])

#  Standard install
bnr.run([GPRINSTALL, "-q", "-p", "--prefix=install", "a2.gpr"])

check_bin("main1")
check_bin("main2")
check_bin("main3")

bnr.run([GPRINSTALL, "-q", "--uninstall", "--prefix=install", "-r", "a2.gpr"])

#  Mode usage install
bnr.run([GPRINSTALL, "-q", "-p", "--prefix=install", "--mode=usage", "a2.gpr"])

check_bin("main1")
check_bin("main2")
check_bin("main3")
