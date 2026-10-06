import os
from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD

bnr = BuilderAndRunner()

result = bnr.simple_run(
    [GPRBUILD, "-P", os.path.join("tree", "main.gpr"), "-p"],
    catch_error=False,
)

#  Report the diagnostic alone: it is prefixed with the project path.
for line in result.out.splitlines():
    if "needs to be encapsulated to be imported by" in line:
        print(line[line.index(": error: ") + 2 :])
