import json

from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD

bnr = BuilderAndRunner()

# Lib1 and Lib2 depend on each other. Lib3 depends on both, so the linker
# needs --start-group/--end-group around them to resolve the cycle. The flag
# is computed while populating Lib3's actions and must reach the link action
# stored in the tree database, not a stale copy of it.

bnr.call([GPRBUILD, "-P", "lib3.gpr", "-p", "-q", "--json-summary"])

with open("jobs.json") as f:
    jobs = json.load(f)

for job in jobs:
    if "[Link]" in job["uid"] and "lib3.gpr" in job["uid"]:
        cmd = job["command"]
        print("start-group:", "--start-group" in cmd)
        print("end-group:", "--end-group" in cmd)
        break
else:
    print("no link action found for lib3")
