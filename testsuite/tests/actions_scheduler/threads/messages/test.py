import os.path
from e3.os.process import Run

from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD

bnr = BuilderAndRunner()

bnr.build(project="test.gpr", args=["-p", "-q"])

# Build the tree project to produce object directories used to store signatures
Run([GPRBUILD, "-p", "-q", os.path.join("tree", "main.gpr")])

FIRST = "first line reported from the action thread"
SECOND = "second line reported from the action thread"


def check(header, ret_code, expected_status):
    proc = bnr.call(["./test", str(ret_code)], quiet=True)
    errors = []

    if proc.status:
        errors.append("test returned " + str(proc.status))
    if FIRST not in proc.out:
        errors.append("missing first line")
    elif SECOND not in proc.out:
        errors.append("missing second line")
    elif proc.out.index(FIRST) > proc.out.index(SECOND):
        errors.append("lines reported out of order")
    if expected_status not in proc.out:
        errors.append("missing '" + expected_status + "'")

    if errors:
        print(header + ": KO: " + ", ".join(errors))
        print(proc.out)
    else:
        print(header + ": OK")


check("action succeeds", 0, "execution succeeded")

check("action fails", 1, "execution failed")
