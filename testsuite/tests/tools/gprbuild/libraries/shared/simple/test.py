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

# Basic check that building mylib.gpr produces mylib.so and that building an
# exe with it only uses mylib.so and not any of the objects contained in it.

run(["gpr2build", "-q", "-Pmylib.gpr", "-p", "--json-summary"])
with open("jobs.json") as fp:
    cntlib = json.load(fp)
run(["gpr2build", "-q", "-Papp.gpr", "-p", "--json-summary"])
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

if os.path.isfile(os.path.join("lib", "libmylib.so")):
    print("mylib has been created, good!")
else:
    print("ERROR: cannot find the lib")

print("bin:")
found = False
for job in cntbin:
    if job["status"] != "SKIPPED":
        if "-lmylib" in job["command"]:
            found = True
            if "pkg.o" in job["command"]:
                print("ERROR: found both -lmylib and pkg.o in the link command")
                print(job["command"])
        if "mylib.a" in job["command"]:
                print("ERROR: The static version of mylib (mylib.a) should not be used in the link command")
        if "mylib.so" in job["command"]:
            print("ERROR: mylib.so should not be explicitly added to the command line. Instead, -L and -l option should be used.");

if not found:
    print("ERROR: cannot find -lmylib in any command issued to build the app")
else:
    print("Ok so far")
run(["./main"])
