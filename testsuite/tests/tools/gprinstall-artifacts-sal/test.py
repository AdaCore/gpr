import os
import subprocess
from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD, GPRINSTALL

bnr = BuilderAndRunner()

# GPRbuild
bnr.run([GPRBUILD, "-p", "-q", "libtst.gpr"], output=True)

# GPRinstall
bnr.run([GPRINSTALL, "--prefix=install", "-p", "libtst.gpr"], output=True)
bnr.run([GPRINSTALL, "--prefix=installm", "-m", "-p", "libtst.gpr"], output=True)

# Test installation
bnr.run([GPRBUILD, "-q", "-f", "-aPinstall/share/gpr", "use/usel.gpr"], output=True)
bnr.run(["./use/obj/main"], output=True)

bnr.run([GPRBUILD, "-q", "-f", "-aPinstallm/share/gpr", "use/usel.gpr"], output=True)
bnr.run(["./use/obj/main"], output=True)
