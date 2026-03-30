import os.path
import glob, os
import shutil
from e3.os.process import Run

from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD

bnr = BuilderAndRunner()
test_number = 1


def test(header, scenario_idx):
    global test_number
    print("================================================================")
    print("Case " + str(test_number) + " - " + header)
    proc = bnr.call(["./test", str(scenario_idx)], quiet=False)

    if proc.status:
        print("Test returned erroneous value: " + str(proc.status))
    print("")
    test_number += 1


def clean_artifacts_and_signatures():
    for f in glob.glob("*.txt"):
        os.remove (f)
    for f in glob.glob("*.json"):
        os.remove(f)
    for f in glob.glob("obj/*"):
        if os.path.isdir(f):
            shutil.rmtree(f)
        else:
            os.remove(f)

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
test("Action n. 6 does not depend on action n. 5", 3)
clean_artifacts_and_signatures()
test("Action n. 3 does not have a valid executable", 4)
clean_artifacts_and_signatures()
test("Action n. 2 is deactivated", 5)
clean_artifacts_and_signatures()
test("Action n. 1 is deactivated so action n. 2 is not executed", 6)
clean_artifacts_and_signatures()
test("Prepare action n.1 signature for next scenario", 7)
test("Action n. 1 is deactivated but has a valid signature, so action n. 2 is executed", 8)

