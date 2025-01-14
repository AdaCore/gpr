import json
import os
import distutils.ccompiler
from testsuite_support.builder_and_runner import BuilderAndRunner


bnr = BuilderAndRunner()
shared_lib_ext = distutils.ccompiler.new_compiler().shared_lib_extension

def run(cmd):
    print("$ " + " ".join(cmd))
    if cmd[0] == "gpr2build":
        bnr.call(cmd)
    else:
        print(bnr.simple_run([cmd], catch_error=True).out)


# Basic check that building mylib1.gpr produces mylib1.so/dll and that building an
# exe with it only uses mylib1.so/dll and not any of the objects contained in it.


run(["gpr2build", "-q", "-Pmylib1.gpr", "-p", "--json-summary"])
with open("jobs.json") as fp:
    cntlib = json.load(fp)
run(["gpr2build", "-q", "-Papp.gpr", "-p", "--json-summary"])
with open("jobs.json") as fp:
    cntbin = json.load(fp)

if os.path.isfile(os.path.join("lib", "libmylib1" + shared_lib_ext)):
    print("mylib1 has been created, good!")
else:
    print("ERROR: cannot find the libmylib1" + shared_lib_ext)

if os.path.isfile(os.path.join("lib", "libmylib2" + shared_lib_ext)):
    print("mylib2 has been created, good!")
else:
    print("ERROR: cannot find the libmylib2" + shared_lib_ext)

found = False
error = False


for job in cntbin:
    if job["status"] != "SKIPPED":
        if "-lmylib1" in job["command"] and "-lmylib2" in job["command"]:
            found = True

            if "pkg1.o" in job["command"] or "pkg2.o" in job["command"]:
                print("ERROR: found both -lmylib1/2 and pkg1/2.o in the link command")
                error = True
        if "mylib1.a" in job["command"] or "mylib2.a" in job["command"]:
            print(
                "ERROR: The static version of the libs should not be used in the link command"
            )
            error = True
        if (
            "mylib1" + shared_lib_ext in job["command"]
            or "mylib2" + shared_lib_ext in job["command"]
        ):
            print(
                "ERROR: mylib1"
                + shared_lib_ext
                + " and mylib2"
                + shared_lib_ext
                + "should not be explicitly added to the command line. Instead, -L and -l option should be used."
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

# Let windows find the dynamic lib
os.environ["PATH"] = os.pathsep.join(
    [os.path.join(os.getcwd(), "lib"), os.environ["PATH"]]
)
run(["./main"])
