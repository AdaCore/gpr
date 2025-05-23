import os
from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPR2BUILD

bnr = BuilderAndRunner()


def run(cmd):
    print("$ " + " ".join(cmd))
    if cmd[0] == GPR2BUILD:
        bnr.call(cmd)
    else:
        print(bnr.simple_run([cmd], catch_error=True).out)


# Basic check that building demo.gpr produces libdemo.a and that building an
# exe with it only uses libdemo.a and not any of the objects contained in it.


run(
    [
        GPR2BUILD,
        "-P" + os.path.join("library_interface", "app.gpr"),
        "-p",
        "-q"
    ])
run(
    [
        GPR2BUILD,
        "-P" + os.path.join("interfaces", "app.gpr"),
        "-p",
        "-q"
    ])
