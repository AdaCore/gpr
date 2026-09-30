import os.path
from e3.os.process import Run

from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD

bnr = BuilderAndRunner()

# Build the test executable
bnr.build(project="test.gpr", args=["-p", "-q", "-j0"])

# Build the tree project to produce object directories used to store signatures
Run([GPRBUILD, "-p", "-q", "-j0", os.path.join("tree", "main.gpr")])

# The child never exits by itself: cleanup must kill and reap it before
# stopping the listeners. The old handler would hang in the Stop rendezvous.
proc = Run(["./test"], timeout=15)

error = any(message not in (proc.out or "") for message in
            ("injected wait failure", "child reaped"))

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
