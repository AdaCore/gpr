import os

from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD


bnr = BuilderAndRunner()

# Exercise external's default without inheriting a value from the environment.
os.environ.pop("EXT_FILE", None)

for name, args in (
    ("unset", []),
    ("empty", ["-XEXT_FILE="]),
    ("nonempty", ["-XEXT_FILE=ext.adb"]),
):
    # The first two cases must ignore the empty filename in the inactive
    # alternative. The last case selects the naming exception instead.
    bnr.check_output([GPRBUILD, "-P", "gpr2_crash.gpr", "-p", "-q"] + args)
    print("EXT_FILE " + name + ": OK")

# Exercise all four naming attributes, with the inactive alternative preceding
# the selected one. The original reproducer exercises the reverse order.
cmd = [GPRBUILD, "-P", "empty_names.gpr", "-p", "-q"]
bnr.check_output(cmd + ["-XENABLE_EMPTY_NAMING=false"])
print("Inactive empty filenames: OK")

# Selecting the same declarations must report errors rather than ignore them.
result = bnr.run(cmd + ["-XENABLE_EMPTY_NAMING=true"])
assert result.status != 0, result.out
for name in ("Body", "Implementation", "Spec", "Specification"):
    diagnostic = (
        'empty filename not allowed for attribute "Naming\'' + name + '"'
    )
    assert diagnostic in result.out, result.out
    print(diagnostic)
assert "ASSERTION_ERROR" not in result.out, result.out
