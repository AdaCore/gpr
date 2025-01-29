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

if os.path.isfile(os.path.join("lib", "libmylib3.a")):
    print("mylib3 has been created, good!")
else:
    print("ERROR: cannot find the libmylib3.a")

found = False
error = False

for job in cntlib:
    if job["status"] != "SKIPPED":
        if "pkg1.o" in job["command"] and "pkg2.o" in job["command"]:
            print("ERROR: Found both pkg1/2.o in the same command")
            error = True
        if "pkg1.o" in job["command"] and "pkg3.o" in job["command"]:
            print("ERROR: Found both pkg1/3.o in the same command")
            error = True
        if "pkg2.o" in job["command"] and "pkg3.o" in job["command"]:
            print("ERROR: Found both pkg2/3.o in the same command")
            error = True

for job in cntbin:
    if job["status"] != "SKIPPED":
        if all(lib in job["command"] for lib in ["mylib1.a","mylib2.a", "mylib3.a"]):
            found = True

            if any(object_file in job["command"] for object_file in ["pkg1.o", "pkg2.o", "pkg3.o"]):
                print("ERROR: found both mylib1/2.a and pkg1/2.o in the link command")
                error = True
        if any(lib in job["command"] for lib in ["-lmylib1", "-lmylib2", "-lmylib3"]):
            print("ERROR: mylib1, mylib2 and mylib3 should be static")
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
