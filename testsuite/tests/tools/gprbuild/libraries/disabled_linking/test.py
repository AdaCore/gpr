import glob
import json
import os

from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD

bnr = BuilderAndRunner()

bnr.run([GPRBUILD, "-P", "main.gpr", "-p", "--json-summary"])

with open("jobs.json") as f:
    jobs = json.load(f)

# Check lib_copy ran with status 0
for job in jobs:
    if "[Library-Files-Copy] Dep (dep.gpr)" in job["uid"] and job["status"] == "0":
        print("lib_copy: OK")
        break
else:
    print("lib_copy: KO")

# Check ada_bind of the Ada main ran with status 0
for job in jobs:
    if "[Ada Bind]" in job["uid"] and "main.gpr" in job["uid"] and job["status"] == "0":
        print("ada_bind: OK")
        break
else:
    print("ada_bind: KO")

# Check that the ada_bind signature lists pkg.ali as an input
sig_files = glob.glob(os.path.join("obj_main", ".ada_bind*.json"))
if sig_files:
    with open(sig_files[0]) as f:
        sig = json.load(f)
    if any("pkg.ali" in item["key"] for item in sig.get("inputs", [])):
        print("ada_bind signature contains pkg.ali: OK")
    else:
        print("ada_bind signature contains pkg.ali: KO")
else:
    print("ada_bind signature file not found: KO")
