import os
import subprocess
from testsuite_support.builder_and_runner import BuilderAndRunner

bnr = BuilderAndRunner()

# GPRbuild
subprocess.run("gprbuild -p -q libtst.gpr", shell=True)

# GPRinstall
subprocess.run("gprinstall --prefix=install -p libtst.gpr", shell=True)
subprocess.run("gprinstall --prefix=installm -m -p libtst.gpr", shell=True)

# Test installation
subprocess.run("gprbuild -q -f -aPinstall/share/gpr use/usel.gpr", shell=True)
bnr.run(["./use/obj/main"], output=True)

subprocess.run("gprbuild -q -f -aPinstallm/share/gpr use/usel.gpr", shell=True)
bnr.run(["./use/obj/main"], output=True)
