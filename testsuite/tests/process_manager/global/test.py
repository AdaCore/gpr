import json
import os.path
import glob, os
from e3.os.process import Run

from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD

bnr = BuilderAndRunner()
test_number = 1


def test(header, scenario_idx):
    global test_number
    print("================================================================")
    print("Case " + str(test_number) + " - " + header)
    proc = bnr.call(["./test", str(scenario_idx)], quiet=True)

    if proc.status:
        print("Test returned erroneous value: " + str(proc.status))
    else:
        print("== Content of jobs.json:")
        json_file = open("jobs.json")
        jobs = json.load(json_file)
        error = False

        for job in jobs:
            status = job["status"]

            # If the provided command does not exist, the process creation may
            # still be successful, but the command will fail later with exit
            # code 127. This behavior is specific to Unix-like systems
            # with bash. To maintain consistency, the status is printed in the
            # same way for both Windows and Unix.
            if status == "FAILED_TO_LAUNCH" or status == "127":
                status = "FAILED_TO_LAUNCH or 127"
            print("uid: '" + job["uid"] + "', status : '" + status + "'")

    print("")
    test_number += 1


def clean_artifacts_and_signatures():
    for f in glob.glob("*.txt"):
        os.remove(f)
    for f in glob.glob("*.json"):
        os.remove(f)
    for f in glob.glob("tree/obj/*.json"):
        os.remove(f)


# Build the driver used during testing
Run([GPRBUILD, "-p", "-q", os.path.join("write_file", "write_file.gpr")])

# Test with instrumentation if any
bnr.build(project="test.gpr", args=["-p", "-q"])

# Build the tree project to produce object directories used to store signatures
Run([GPRBUILD, "-p", "-q", os.path.join("tree", "main.gpr")])

test(
    "No errors",
    1,
)
test("No errors with already existing artifacts", 1)
clean_artifacts_and_signatures()
test("Action n. 2 returns an erroneous code", 2)
clean_artifacts_and_signatures()
test("Action n. 5 does not depend on action n. 4", 3)
clean_artifacts_and_signatures()
test("Action n. 3 does not have a valid executable", 4)
