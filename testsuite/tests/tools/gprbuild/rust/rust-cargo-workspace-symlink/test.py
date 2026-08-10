import os
from e3.env import Env
from pathlib import Path
from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD

bnr = BuilderAndRunner()

if "windows" in Env().host.platform:
    exe = ".exe"
else:
    exe = ""


def run(cmd):
    if cmd[0] == GPRBUILD:
        bnr.call(cmd)
    else:
        print(bnr.simple_run([cmd], catch_error=True).out)


#  Reach the project through a symbolic link, so that Cargo.Root spells the
#  manifest differently from the way "cargo metadata" reports it.
Path("link").symlink_to("tree")

run([GPRBUILD, "-q", "-P", os.path.join("link", "hello_from_ada.gpr"), "-p"])
run([os.path.join("link", "hello_from_ada" + exe)])

os.unlink("link")
