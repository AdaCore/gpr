import os
from testsuite_support.builder_and_runner import BuilderAndRunner

bnr = BuilderAndRunner()


def run(cmd):
    print("$ " + " ".join(cmd))
    if cmd[0] == "gpr2build":
        bnr.call(cmd)
    else:
        print(bnr.simple_run([cmd], catch_error=True).out)


# Basic check that building demo.gpr produces libdemo.a and that building an
# exe with it only uses libdemo.a and not any of the objects contained in it.


run(
    [
        "gpr2build",
        "-P" + os.path.join("library_interface", "app.gpr"),
        "-p",
        "-j1"
    ])
run(
    [
        "gpr2build",
        "-P" + os.path.join("interfaces", "app.gpr"),
        "-p",
        "-j1"
    ])
