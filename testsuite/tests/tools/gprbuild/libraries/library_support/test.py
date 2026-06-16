from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD, GPRCLEAN

import os
from pathlib import Path
import stat
from e3.env import Env

bnr = BuilderAndRunner()

def run(cmd):
    print("$ " + " ".join(cmd))
    if cmd[0] in (GPRBUILD, GPRCLEAN):
        bnr.call(cmd)
    else:
        print(bnr.simple_run([cmd], catch_error=True).out)

def chmod_remove_write(path):
    print(f"$ chmod -w {path}")
    current = stat.S_IMODE(os.stat(path).st_mode)
    os.chmod(path, current & ~0o222)

if "windows" in Env().host.platform:
    exe=".exe"
else:
    exe=""

run([GPRBUILD, "-P", "prj.gpr", "-XLIB_TYPE=static", "-XSUPPORT_TYPE=none"]) # should not build without construct
run([GPRBUILD, "-P", "prj.gpr", "-j1", "-XLIB_TYPE=static", "-XSUPPORT_TYPE=none", "-XCONSTRUCT_TYPE=true"])
run([f"./main{exe}"])
run([GPRCLEAN, "-p", "-r", "-P", "prj.gpr", "-XLIB_TYPE=static", "-XSUPPORT_TYPE=none", "-XCONSTRUCT_TYPE=true"])

run([GPRBUILD, "-P", "prj.gpr", "-j1", "-XLIB_TYPE=static", "-XSUPPORT_TYPE=static_only"])
run([f"./main{exe}"])
run([GPRCLEAN, "-p", "-r", "-P", "prj.gpr", "-XLIB_TYPE=static", "-XSUPPORT_TYPE=static_only"])

run([GPRBUILD, "-P", "prj.gpr", "-j1", "-XLIB_TYPE=static", "-XSUPPORT_TYPE=full"])
run([f"./main{exe}"])
run([GPRCLEAN, "-p", "-r", "-P", "prj.gpr", "-XLIB_TYPE=static", "-XSUPPORT_TYPE=full"])

run([GPRBUILD, "-P", "prj.gpr", "-XLIB_TYPE=shared", "-XSUPPORT_TYPE=none"]) # Should not build without construct
run([GPRBUILD, "-P", "prj.gpr", "-j1", "-XLIB_TYPE=shared", "-XSUPPORT_TYPE=none", "-XCONSTRUCT_TYPE=true"])
run([f"./main{exe}"])
run([GPRCLEAN, "-p", "-r", "-P", "prj.gpr", "-XLIB_TYPE=shared", "-XSUPPORT_TYPE=none", "-XCONSTRUCT_TYPE=true"])

run([GPRBUILD, "-P", "prj.gpr", "-XLIB_TYPE=shared", "-XSUPPORT_TYPE=static_only"]) # Should not build without construct
run([GPRBUILD, "-P", "prj.gpr", "-j1", "-XLIB_TYPE=shared", "-XSUPPORT_TYPE=static_only", "-XCONSTRUCT_TYPE=true"])
run([f"./main{exe}"])
run([GPRCLEAN, "-p", "-r", "-P", "prj.gpr", "-XLIB_TYPE=shared", "-XSUPPORT_TYPE=static_only", "-XCONSTRUCT_TYPE=true"])

run([GPRBUILD, "-P", "prj.gpr", "-j1", "-XLIB_TYPE=shared", "-XSUPPORT_TYPE=full"])

if "windows" in Env().host.platform:
    Env().add_path(os.path.join(os.getcwd(), "tree", "lib"))

run([f"./main{exe}"])
run([GPRCLEAN, "-p", "-r", "-P", "prj.gpr", "-XLIB_TYPE=shared", "-XSUPPORT_TYPE=full"])
