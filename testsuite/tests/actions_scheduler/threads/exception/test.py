import os.path
from e3.os.process import Run

from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD

bnr = BuilderAndRunner()

# Build the test executable
bnr.build(project="test.gpr", args=["-p", "-q"])

# Build the tree project to produce object directories used to store signatures
Run([GPRBUILD, "-p", "-q", os.path.join("tree", "main.gpr")])
proc = bnr.call(["./test"], quiet=True)

error = False

if "test exception raised by action" not in proc.out or "Error: action execution should have failed" in proc.out:
    error = True

if proc.status:
    error = True
    print("Test returned erroneous value: " + str(proc.status))

if error:
    print("KO")
else:
    print("OK")
