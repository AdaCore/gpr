import json
import os
import shutil

from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD

bnr = BuilderAndRunner()

# The first build produces the binder sources: --no-sal-binding then reuses
# them instead of running the binder again, as gprbuild does.

bnr.call([GPRBUILD, "-Plib.gpr", "-p", "-q"])

# Change a source so that the bind signature no longer matches, and remove
# the library so that the archive has to run again. Only the no-op state can
# keep the archive unblocked now.

shutil.copy(
    os.path.join("src", "pub.adb.new-version"), os.path.join("src", "pub.adb")
)

lib = os.path.join("libdir", "liblib.a")
os.remove(lib)

bnr.call(
    [GPRBUILD, "-Plib.gpr", "-p", "-q", "--no-sal-binding", "--json-summary"]
)

# Bind and post-bind are no-ops: never executed, yet reported successful, so
# the whole link phase behind them still runs.

for job in sorted(json.load(open("jobs.json")), key=lambda j: j["uid"]):
    print(f"{job['uid']}: {job['status']}")

print(f"library rebuilt: {os.path.exists(lib)}")
