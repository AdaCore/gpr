import json
import os
import distutils.ccompiler
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


def test(test_dir):
    run(
        [
            GPRBUILD,
            "-q",
            "-P" + os.path.join(test_dir, "demo.gpr"),
            "-p",
            "--json-summary",
            "-j1",
        ]
    )
    with open(os.path.join(test_dir, "jobs.json")) as fp:
        cntlib = json.load(fp)

    run(
        [
            GPRBUILD,
            "-q",
            "-P" + os.path.join(test_dir, "app.gpr"),
            "-p",
            "--json-summary",
            "-j1",
        ]
    )

    if os.path.isfile(os.path.join(test_dir, "lib", "libdemo.a")):
        print("lib [" + test_dir + "] has been created")
    else:
        print("ERROR: cannot find the lib")

    # Check that the linker options and the binder object file content are
    # merged in a second object file, called
    # o__<lib_name>.o
    binder_obj_with_linker_options = os.path.join(
        test_dir, "obj", "lib", "o__demo.o"
    )
    if os.path.isfile(binder_obj_with_linker_options):
        objdump_cmd = [
            "objdump",
            "-s",
            "--section=.GPR.linker_options",
            binder_obj_with_linker_options,
        ]
        if (
            not "Contents of section .GPR.linker_options:"
            in bnr.simple_run(objdump_cmd, catch_error=True).out
        ):
            print(
                "ERROR: "
                + binder_obj_with_linker_options
                + " does not contain the '.GPR.linker_options' section"
            )
    else:
        print("ERROR: " + binder_obj_with_linker_options + " is missing")

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
                if "foo.ali" not in job["command"]:
                    print(
                        "ERROR: missing foo.ali, not in interface but should be given to gnatbind"
                    )
                    print(job["command"])
            elif "ar csr" in job["command"]:
                if "p__demo.o" not in job["command"]:
                    print(
                        "ERROR: p__demo.o is missing for the ar csr command"
                    )
                if "o__demo.o" not in job["command"]:
                    print(
                        "ERROR: o__demo.o is missing from the ar csr command"
                    )

    if not found:
        print("ERROR: cannot find gnatbind in any command issued to build the app")
    else:
        print("Ok so far")
    run([os.path.join(test_dir, "main")])

    run(
        [
            GPRBUILD,
            "-P" + os.path.join(test_dir, "invalid_app_foo.gpr"),
            "-p",
            "--json-summary",
            "-j1",
        ]
    )

    run(
        [
            GPRBUILD,
            "-P" + os.path.join(test_dir, "invalid_app_mult.gpr"),
            "-p",
            "--json-summary",
            "-j1",
        ]
    )
    print("")


# Basic check that building demo.gpr produces libdemo.a and that building an
# exe with it only uses libdemo.a and not any of the objects contained in it.

test("library_interface")
test("interfaces")
