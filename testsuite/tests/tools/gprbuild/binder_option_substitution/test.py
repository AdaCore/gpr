import os

from e3.env import Env
from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD

bnr = BuilderAndRunner()

exe = ".exe" if "windows" in Env().host.platform else ""


def binder_options(path):
    """The options gnatbind listed in the body it generated."""
    options = []
    reading = False

    for line in open(path):
        if "BEGIN Object file/option list" in line:
            reading = True
        elif "END Object file/option list" in line:
            break
        elif reading:
            options.append(line.split("--", 1)[1].strip())

    return options


bnr.call([GPRBUILD, "-q", "-p", "-P", os.path.join("tree", "prj.gpr")])

options = binder_options(os.path.join("tree", "obj", "b__main.adb"))
print(
    "an option to substitute was listed:",
    any(option in ("-static", "-shared") for option in options),
)

main = os.path.join("tree", "main" + exe)
print(bnr.simple_run([main], catch_error=True).out)
