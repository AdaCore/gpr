import os
from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD

bnr = BuilderAndRunner()

out = bnr.simple_run(
    [GPRBUILD, "-P", os.path.join("tree", "hello_from_ada.gpr"), "-p"],
    catch_error=False,
).out

#  Report the diagnostic alone: it is prefixed with the project path.
for line in out.splitlines():
    if "Rust target triple" in line:
        print(line[line.index("Rust target triple") :])
