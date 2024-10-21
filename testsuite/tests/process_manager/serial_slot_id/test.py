import glob
import json
import os
import re
from e3.os.process import Run

from testsuite_support.builder_and_runner import BuilderAndRunner, GPRBUILD

bnr = BuilderAndRunner()
test_number = 1


# Build the driver used during testing
Run([GPRBUILD, "-p", "-q", os.path.join("write_file", "write_file.gpr")])

# Cleanup signatures and output artifacts
for f in glob.glob("*.txt"):
    os.remove(f)
for f in glob.glob("*.json"):
    os.remove(f)
for f in glob.glob("tree/*.json"):
    os.remove(f)

# Test with instrumentation if any
bnr.build(project="test.gpr", args=["-p", "-q"])
proc = bnr.call(["./test"])

if proc.status:
    print("Test returned erroneous value: " + str(proc.status))
else:
    print("== Content of jobs.json:")
    json_file = open("jobs.json")
    exec_log = json.load(json_file)
    jobs = {}
    for log in exec_log:
        m = re.match(r"Write_File \'([0-9]*)\'.*", log['uid'])
        uid = int(m.group(1))
        jobs[uid] = {"uid": log["uid"],
                     "status": log["status"],
                     "stdout": log["stdout"],
                     "stderr": log["stderr"]}
    error = False

    for key, val in sorted(jobs.items()):
        out = val["stdout"]
        err = val["stderr"]

        print(f"{val['uid']}:")
        print(f"  status: {val['status']}")

        if len(out) > 0:
            print(f"  stdout: {out}")
        if len(err) > 0:
            print(f"  stderr: {err}")
