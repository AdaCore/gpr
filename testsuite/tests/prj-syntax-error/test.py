import os
import subprocess

# GPRbuild
subprocess.run("gprbuild -p -q prj.gpr", shell=True)

# GPRls
subprocess.run("gprls prj.gpr", shell=True)
