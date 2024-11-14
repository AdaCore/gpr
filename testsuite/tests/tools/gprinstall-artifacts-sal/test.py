import os
import subprocess
from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD, GPRINSTALL

bnr = BuilderAndRunner()

# GPRbuild
bnr.run([GPRBUILD, "-p", "-q", "libtst.gpr"])

# GPRinstall
bnr.run([GPRINSTALL, "--prefix=install", "-p", "libtst.gpr"])
bnr.run([GPRINSTALL, "--prefix=installm", "-m", "-p", "libtst.gpr"])

# Test installation
bnr.run([GPRBUILD, "-q", "-f", "-aPinstall/share/gpr", "use/usel.gpr"])
bnr.run(["./use/obj/main"], output=True)

bnr.run([GPRBUILD, "-q", "-f", "-aPinstallm/share/gpr", "use/usel.gpr"])
bnr.run(["./use/obj/main"], output=True)
