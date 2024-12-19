import json
import os
from e3.fs import mv
from testsuite_support.builder_and_runner import BuilderAndRunner

bnr = BuilderAndRunner()


def run(cmd):
    print("$ " + " ".join(cmd))
    if cmd[0] == "gpr2build":
        bnr.call(cmd)
    else:
        print(bnr.simple_run([cmd], catch_error=True).out)


# Basic check that building mylib1.gpr produces mylib1.so and that building an
# exe with it only uses mylib1.so and not any of the objects contained in it.

run(["gpr2build", "-q", "-Pmylib1.gpr", "-p", "--json-summary"])
with open("jobs.json") as fp:
    cntlib = json.load(fp)
run(["gpr2build", "-q", "-Papp.gpr", "-p", "--json-summary"])
with open("jobs.json") as fp:
    cntbin = json.load(fp)


print("lib:")
for job in cntlib:
    print("uid: '" + job["uid"] + "', status : '" + job["status"] + "'")

if os.path.isfile(os.path.join("lib", "libmylib1.a")):
    print("mylib1 has been created, good!")
else:
    print("ERROR: cannot find the libmylib1.a")

if os.path.isfile(os.path.join("lib", "libmylib2.so")):
    print("mylib2 has been created, good!")
else:
    print("ERROR: cannot find the libmylib2.so")

print("bin:")
found = False
error = False
for job in cntbin:
    if job["status"] != "SKIPPED":
        if "mylib1.a" in job["command"] and "-lmylib2" in job["command"]:
            found = True

            if "pkg1.o" in job["command"] or "pkg2.o" in job["command"]:
                print(
                    "ERROR: found both mylib1.a , -lmylib2 and pkg1/2.o in the link command"
                )
                error = True
        if "-lmylib1" in job["command"] or "mylib2.a" in job["command"]:
            print("ERROR: mylib1 should be static, and mylib2 shared")
            error = True

        if "mylib2.so" in job["command"]:
            print(
                "ERROR: mylib2.so should not be explicitly added to the command line. Instead, -L and -l option should be used."
            )
            error = True

if error:
    print(cntbin)

if not found:
    print(
        "ERROR: cannot find -lmylib1 and -lmylib2 in any command issued to build the app"
    )
else:
    print("Ok so far")
run(["./main"])
