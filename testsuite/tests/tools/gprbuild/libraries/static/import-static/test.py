import json
import os
from testsuite_support.builder_and_runner import BuilderAndRunner

bnr = BuilderAndRunner()

def run(cmd):
    print("$ " + " ".join(cmd))
    if cmd[0] == "gpr2build":
        bnr.call(cmd)
    else:
        print(bnr.simple_run([cmd], catch_error=True).out)

run(["gpr2build", "-q", "-Pmylib1.gpr", "-p", "--json-summary"])
with open("jobs.json") as fp:
    cntlib = json.load(fp)
run(["gpr2build", "-q", "-Papp.gpr", "-p", "--json-summary"])
with open("jobs.json") as fp:
    cntbin = json.load(fp)


if os.path.isfile(os.path.join("lib", "libmylib1.a")):
    print("mylib1 has been created, good!")
else:
    print("ERROR: cannot find the libmylib1.a")

if os.path.isfile(os.path.join("lib", "libmylib2.a")):
    print("mylib2 has been created, good!")
else:
    print("ERROR: cannot find the libmylib2.a")

found = False
error = False
for job in cntbin:
    if job["status"] != "SKIPPED":
        if "mylib1.a" in job["command"] and "mylib2.a" in job["command"]:
            found = True

            if "pkg1.o" in job["command"] or "pkg2.o" in job["command"]:
                print("ERROR: found both mylib1/2.a and pkg1/2.o in the link command")
                error = True
        if "-lmylib1" in job["command"] or "-lmylib2" in job["command"]:
            print("ERROR: mylib1 and mylib2 should be static")
            error = True

if error:
    print(cntbin)

if not found:
    print(
        "ERROR: cannot find mylib1.a and mylib2.a in any command issued to build the app"
    )
else:
    print("Ok so far")
run(["./main"])
