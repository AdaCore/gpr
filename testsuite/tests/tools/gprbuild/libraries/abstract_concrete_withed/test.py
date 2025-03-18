import json
import os
from e3.fs import mv
from testsuite_support.builder_and_runner import BuilderAndRunner

bnr = BuilderAndRunner()

def run(cmd):
    print("$ " + " ".join(cmd));
    if cmd[0] == "gpr2build":
        bnr.call(cmd)
    else:
        print(bnr.simple_run([cmd], catch_error=True).out)

# calling gprbuild on an abstract project that withes libraries should still
# build the libraries

run(["gpr2build", "-q", "-Pdemo.gpr", "-p", "--json-summary", "-j1"])
with open("jobs.json") as fp:
    cnt = json.load(fp)

uids = [job["uid"] for job in cnt]

for uid in sorted(uids):
    if "[Archive]" in uid:
        print(uid)

run(["gpr2build", "-q", "-Papp.gpr", "-p"])
run(["./main"])
