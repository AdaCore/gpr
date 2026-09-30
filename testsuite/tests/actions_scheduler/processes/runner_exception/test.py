import os.path
from e3.os.process import Run

from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD

bnr = BuilderAndRunner()

# Build the test executable
bnr.build(project="test.gpr", args=["-p", "-q"])

# Build the tree project to produce object directories used to store signatures
Run([GPRBUILD, "-p", "-q", os.path.join("tree", "main.gpr")])

# Bounded: the runner used to die here, leaving its slot taken and the
# scheduler waiting on it for good. It now reports the failure instead.
proc = Run(["./test"], timeout=120)

error = False

if proc.status is None:
    error = True
    print("Test timed out: the runner did not shut its tasks down")

if "scheduler returned" not in (proc.out or ""):
    error = True
    print("Scheduler did not return")

if "Error: action execution should have failed" in (proc.out or ""):
    error = True

if proc.status:
    error = True
    print("Test returned erroneous value: " + str(proc.status))

if error:
    print(proc.out)
    print("KO")
else:
    print("OK")
