import glob
import os
from e3.env import Env
from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD

bnr = BuilderAndRunner()

if "windows" in Env().host.platform:
    exe = ".exe"
else:
    exe = ""

BODY = os.path.join("tree", "src_mathlib", "math_lib.adb")


def build_and_run():
    bnr.call([GPRBUILD, "-q", "-P", os.path.join("tree", "main.gpr"), "-p"])

    matches = glob.glob(
        os.path.join(
            "tree", "rust", "target", "*", "release", "main_from_rust" + exe
        )
    )
    print(bnr.simple_run([matches[0]], catch_error=True).out)


build_and_run()

#  Change what the Ada library computes and nothing else: the Rust sources,
#  the manifest and the flags handed to Cargo all stay as they were, so the
#  only difference between the two builds is the library linked against.
with open(BODY) as source:
    body = source.read()

with open(BODY, "w") as source:
    source.write(body.replace("A + B", "A - B"))

build_and_run()
