import json
import os
from e3.fs import mv
from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRCLEAN

bnr = BuilderAndRunner()


def run(cmd):
    print("$ " + " ".join(cmd))
    if cmd[0] == "gpr2build":
        bnr.call(cmd)
    else:
        print(bnr.simple_run([cmd], catch_error=True).out)


def test(test_dir):
    run(
        [
            "gpr2build",
            "-q",
            "-P" + os.path.join(test_dir, "demo.gpr"),
            "-p",
            "--json-summary",
        ]
    )
    with open(os.path.join(test_dir, "jobs.json")) as fp:
        cntlib = json.load(fp)

    run(
        [
            "gpr2build",
            "-q",
            "-P" + os.path.join(test_dir, "app.gpr"),
            "-p",
            "--json-summary",
        ]
    )

    if os.path.isfile(os.path.join(test_dir, "lib", "libdemo.a")):
        print("lib [" + test_dir + "] has been created")
    else:
        print("ERROR: cannot find the lib")

    for job in cntlib:
        print("uid: '" + job["uid"] + "', status : '" + job["status"] + "'")

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
        "gpr2build",
        "-P" + os.path.join(test_dir,"invalid_app.gpr"),
        "-p",
        "--json-summary",
    ]
)

# Basic check that building demo.gpr produces libdemo.a and that building an
# exe with it only uses libdemo.a and not any of the objects contained in it.

test("library_interface")
test("interfaces")