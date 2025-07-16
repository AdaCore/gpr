import json
import os
import distutils.ccompiler
from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD

bnr = BuilderAndRunner()
shared_lib_ext = distutils.ccompiler.new_compiler().shared_lib_extension

def run(cmd):
    print("$ " + " ".join(cmd));
    if cmd[0] == GPRBUILD:
        bnr.call(cmd)
    else:
        print(bnr.simple_run([cmd], catch_error=True).out)

# Basic check that building mylib.gpr produces mylib.so/dll and that building an
# exe with it only uses mylib.so/dll and not any of the objects contained in it.

run([GPRBUILD, "-q", "-Pmylib.gpr", "-p", "--json-summary", "-j1"])
with open("jobs.json") as fp:
    cntlib = json.load(fp)
run([GPRBUILD, "-q", "-Papp.gpr", "-p", "--json-summary", "-j1"])
with open("jobs.json") as fp:
    cntbin = json.load(fp)


if os.path.isfile(os.path.join("lib", "libmylib" + shared_lib_ext)):
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
        if "mylib" + shared_lib_ext in job["command"]:
            print("ERROR: mylib" + shared_lib_ext + " should not be explicitly added to the command line. Instead, -L and -l option should be used.");

if not found:
    print("ERROR: cannot find -lmylib in any command issued to build the app")
else:
    print("Ok so far")

# Let windows find the dynamic lib
os.environ["PATH"] = os.pathsep.join(
    [os.path.join(os.getcwd(), "lib"), os.environ["PATH"]]
)
run(["./main"])
