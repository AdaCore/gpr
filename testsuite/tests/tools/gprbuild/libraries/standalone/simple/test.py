import json
import os
import distutils.ccompiler
from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPR2BUILD

bnr = BuilderAndRunner()
shared_lib_ext = distutils.ccompiler.new_compiler().shared_lib_extension


def run(cmd):
    print("$ " + " ".join(cmd))
    if cmd[0] == GPR2BUILD:
        bnr.call(cmd)
    else:
        print(bnr.simple_run([cmd], catch_error=True).out)


def test(test_dir):
    run(
        [
            GPR2BUILD,
            "-q",
            "-P" + os.path.join(test_dir, "demo.gpr"),
            "-p",
            "--json-summary",
            "-j1"
        ]
    )
    with open(os.path.join(test_dir, "jobs.json")) as fp:
        cntlib = json.load(fp)

    run(
        [
            GPR2BUILD,
            "-q",
            "-P" + os.path.join(test_dir, "app.gpr"),
            "-p",
            "--json-summary",
            "-j1"
        ]
    )

    if os.path.isfile(os.path.join(test_dir, "lib", "libdemo.a")):
        print("lib [" + test_dir + "] has been created")
    else:
        print("ERROR: cannot find the lib")

    found = False
    for job in cntlib:
        if job["status"] != "SKIPPED":
            if "gnatbind" in job["command"]:
                found = True
                if "sub.ali" not in job["command"]:
                    print("ERROR: sub.ali missing from gnatbind command")
                    print(job["command"])
                if "add.ali" not in job["command"]:
                    print("ERROR: add.ali missing from gnatbind command")
                    print(job["command"])
                if "mult.ali" in job["command"]:
                    print(
                        "ERROR: mult.ali should not be in gnatbind command as it is not part of the interface"
                    )
                    print(job["command"])
                if "foo.ali" in job["command"]:
                    print(
                        "ERROR: foo.ali should not be in gnatbind command as it is not part of the interface"
                    )
                    print(job["command"])
    if not found:
        print("ERROR: cannot find gnatbind in any command issued to build the app")
    else:
        print("Ok so far")
    run([os.path.join(test_dir, "main")])

    run(
        [
            GPR2BUILD,
            "-P" + os.path.join(test_dir, "invalid_app_foo.gpr"),
            "-p",
            "--json-summary",
            "-j1"
        ]
    )

    run(
        [
            GPR2BUILD,
            "-P" + os.path.join(test_dir, "invalid_app_mult.gpr"),
            "-p",
            "--json-summary",
            "-j1"
        ]
    )
    print("")


# Basic check that building demo.gpr produces libdemo.a and that building an
# exe with it only uses libdemo.a and not any of the objects contained in it.

test("library_interface")
test("interfaces")
