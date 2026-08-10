import os
import re
from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD

bnr = BuilderAndRunner()

out = bnr.simple_run(
    [GPRBUILD, "-P", os.path.join("tree", "hello_from_ada.gpr"), "-p"],
    catch_error=False,
).out

#  Report the diagnostic alone. What precedes it is the project file, and the
#  manifest it names is an absolute path.
for line in out.splitlines():
    if ": error: " in line:
        print(re.sub(r"[^ ]*Cargo\.toml", "Cargo.toml",
                     line[line.index(": error: ") + 2 :]))
