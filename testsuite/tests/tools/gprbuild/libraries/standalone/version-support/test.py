import json
import os
import distutils.ccompiler
from e3.env import Env
from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD

bnr = BuilderAndRunner()
shared_lib_ext = distutils.ccompiler.new_compiler().shared_lib_extension


def run(cmd):
    print("$ " + " ".join(cmd))
    if cmd[0] == GPRBUILD:
        bnr.call(cmd)
    else:
        print(bnr.simple_run([cmd], catch_error=True).out)


def test(gpr):
    run(
        [
            GPRBUILD,
            "-q",
            f"-P{gpr}",
            "-p",
        ]
    )
    run(["./main"])


env = Env()
if env.host.os.name == "windows":
    # need to add the dll to the path
    env.add_dll_path(os.path.join(os.getcwd(), "lib"))

test("app_c")
test("app_ada")
