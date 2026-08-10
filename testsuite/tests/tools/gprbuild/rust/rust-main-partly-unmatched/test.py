import os
from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD

bnr = BuilderAndRunner()

out = bnr.simple_run(
    [GPRBUILD, "-P", os.path.join("tree", "tools.gpr"), "-p"],
    catch_error=False,
).out

#  Report the diagnostic alone: the line it sits on is prefixed with the
#  project path.
for line in out.splitlines():
    if "the Cargo package" in line:
        print(line[line.index("the Cargo package") :])
