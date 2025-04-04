import json
import os.path
from e3.env import Env
from e3.os.process import Run

from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPR2BUILD

bnr = BuilderAndRunner()
test_number = 1


def test(header):
    global test_number
    print("================================================================")
    print("Case " + str(test_number) + " - " + header)
    proc = bnr.call(
        [GPR2BUILD, "-P", os.path.join("tree", "main.gpr"), "-p", "--json-summary", "-j1"])

    if proc.status:
        print("Test return value: " + str(proc.status))
    else:
        print("== Content of jobs.json:")
        json_file = open(os.path.join("tree", "jobs.json"))
        jobs = json.load(json_file)
        error = False

        for job in jobs:
            print(
                "uid: '"
                + job["uid"]
                + "', status : '"
                + job["status"]
                + "', stdout: '"
                + job["stdout"]
                + "', stderr: '"
                + job["stderr"]
                + "'"
            )
            if job["status"] != "SKIPPED" and job["status"] != "0":
                error = True

        if error:
            print("Error detected in jobs.json")
        else:
            print("== Executable output:")
            print(Run([os.path.join("tree", "obj", "main")]).out.strip())

    print("")
    test_number += 1


def testsuite(file_path):
    with open(file_path, "rb") as file:
        file_content = file.read()

    os.remove(file_path)
    test("Missing " + os.path.basename(file_path))

    if not os.path.exists(file_path):
        with open(file_path, "wb") as file:
            file.write(file_content)

    # Restore state of the project after a correct compilation
    Run([GPR2BUILD, "-P", os.path.join("tree", "main.gpr"), "-p", "-j1"])

    with open(file_path, "a") as file:
        file.write("--  Comment that will not prevent a compilation for Ada files")

    test("Modified " + os.path.basename(file_path) + " (comments only)")

    # Restore state of the project after a correct compilation
    Run([GPR2BUILD, "-P", os.path.join("tree", "main.gpr"), "-p", "-j1"])


test("Build from scratch")

testsuite(os.path.join("tree", "obj", "main.o"))
testsuite(os.path.join("tree", "obj", "main.ali"))
testsuite(os.path.join("tree", "obj", "pkg.o"))
testsuite(os.path.join("tree", "obj", "pkg.ali"))
testsuite(os.path.join("tree", "obj", "dep_two.o"))
testsuite(os.path.join("tree", "obj", "dep_two.ali"))
testsuite(os.path.join("tree", "obj", "b__main.o"))
testsuite(os.path.join("tree", "obj", "b__main.ali"))

if "windows" in Env().host.platform:
    exe=".exe"
else:
    exe=""

testsuite(os.path.join("tree", "obj", "main" + exe))
