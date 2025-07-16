import json
import os
from e3.fs import mv
from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD

bnr = BuilderAndRunner()

def run(cmd):
    print("$ " + " ".join(cmd));
    if cmd[0] == GPRBUILD:
        bnr.call(cmd)
    else:
        print(bnr.simple_run([cmd], catch_error=True).out)

# Basic check that building demo.gpr produces libdemo.a and that building an
# exe with it only uses libdemo.a and not any of the objects contained in it.

run([GPRBUILD, "-q", "-Pdemo.gpr", "-p", "--json-summary"])
with open("jobs.json") as fp:
    cntlib = json.load(fp)
run([GPRBUILD, "-q", "-Papp.gpr", "-p", "--json-summary"])
with open("jobs.json") as fp:
    cntbin = json.load(fp)


print("lib:")
for job in cntlib:
    print(
        "uid: '"
        + job["uid"]
        + "', status : '"
        + job["status"]
        + "'"
    )

if os.path.isfile(os.path.join("lib", "libdemo.a")):
    print("lib has been created, good!")
else:
    print("ERROR: cannot find the lib")

print("bin:")
found = False
for job in cntbin:
    if job["status"] != "SKIPPED":
        if "libdemo.a" in job["command"]:
            found = True
            if "pkg.o" in job["command"]:
                print("ERROR: found both libdemo.a and pkg.o in the link command")
                print(job["command"])
if not found:
    print("ERROR: cannot find libdemo.a in any command issued to build the app")
else:
    print("Ok so far")
run(["./main"])
