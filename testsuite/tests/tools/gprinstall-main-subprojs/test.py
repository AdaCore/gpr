import os
import subprocess
from testsuite_support.builder_and_runner import BuilderAndRunner

def check_bin(main):
    prefix='install/bin/'
    if os.path.exists(prefix + main) or os.path.exists(prefix + main + '.exe'):
        print("OK: " + main)
    else:
        print("NOK: "  + main + " not installed")

# For default
subprocess.run("gprbuild -p -q a2.gpr", shell=True)

#  Standard install
subprocess.run("gprinstall -q  -p --prefix=install a2.gpr",
               shell=True)

check_bin("main1")
check_bin("main2")
check_bin("main3")

subprocess.run("gprinstall -q --uninstall --prefix=install -r a2.gpr",
               shell=True)

#  Mode usage install
subprocess.run("gprinstall -q  -p --prefix=install --mode=usage a2.gpr",
               shell=True)

check_bin("main1")
check_bin("main2")
check_bin("main3")
