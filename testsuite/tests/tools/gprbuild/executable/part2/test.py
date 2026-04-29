import json
import os.path
from e3.env import Env
from e3.os.process import Run

from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD

bnr = BuilderAndRunner()
test_number = 1

COMPILE_MAIN = "[Ada Compile] main.adb"
COMPILE_PKG = "[Ada Compile] pkg.adb"
COMPILE_DEP_TWO = "[Ada Compile] dep_two.adb"
BIND_MAIN = "[Ada Bind] main"
POST_BIND_MAIN = "[Ada Post-Bind] b__main.adb"
LINK_MAIN = "[Link] main"

# Expected job statuses per artifact for testsuite() calls.
# "missing": used for the "missing" part of the tests. None means build failure
#   expected, otherwise set of active job uid patterns.
# "modified": used for the "modified" part of the tests. Set of active job uid
#   patterns (all others should be SKIPPED).
EXPECTATIONS = {
    "main": {
        "missing": {LINK_MAIN},
        "modified": {LINK_MAIN},
    },
    "main.exe": {
        "missing": {LINK_MAIN},
        "modified": {LINK_MAIN},
    },
    "main.adb": {
        "missing": None,
        "modified": {COMPILE_MAIN, BIND_MAIN},
    },
    "pkg.adb": {
        "missing": None,
        "modified": {COMPILE_PKG, BIND_MAIN},
    },
    "pkg.ads": {
        "missing": None,
        "modified": {COMPILE_MAIN, COMPILE_PKG, BIND_MAIN},
    },
    "dep_two.adb": {
        "missing": None,
        "modified": {COMPILE_DEP_TWO, BIND_MAIN},
    },
    "dep_two.ads": {
        "missing": None,
        "modified": {COMPILE_DEP_TWO, COMPILE_PKG, BIND_MAIN},
    },
    "b__main.ads": {
        "missing": {BIND_MAIN},
        "modified": {BIND_MAIN},
    },
    "b__main.adb": {
        "missing": {BIND_MAIN},
        "modified": {BIND_MAIN},
    },
}


def test(header, expected_active=None, expected_failure=False):
    """Run gprbuild and check job statuses.

    expected_active: set of uid substrings for jobs expected to run (status '0').
                     If None, all jobs should have status '0'.
    expected_failure: if True, the build is expected to fail (non-zero status).
    """
    global test_number
    ok = True
    proc = bnr.call(
        [GPRBUILD, "-P", os.path.join("tree", "main.gpr"), "-p", "--json-summary", "-j1", "-q"], quiet=True)

    if expected_failure:
        if proc.status == 0:
            print("  expected build failure but build succeeded")
            ok = False
    elif proc.status:
        print("  unexpected build failure (status " + str(proc.status) + ")")
        ok = False
    else:
        with open(os.path.join("tree", "jobs.json")) as json_file:
            jobs = json.load(json_file)

        for job in jobs:
            uid = job["uid"]
            status = job["status"]
            if expected_active is None:
                if status != "0":
                    print("  job '" + uid + "' has status '" + status
                          + "', expected '0'")
                    ok = False
                    break
            else:
                is_active = any(pat in uid for pat in expected_active)
                expected_status = "0" if is_active else "SKIPPED"
                if status != expected_status:
                    print("  job '" + uid + "' has status '" + status
                          + "', expected '" + expected_status + "'")
                    ok = False
                    break

    print(header + ": " + ("OK" if ok else "KO"))
    test_number += 1


def testsuite(file_path):
    """Test rebuild behavior when a build artifact is missing or modified.

    Removes file_path and checks that gprbuild triggers the expected actions,
    then restores it, appends a comment, and checks that only the expected
    recompilations occur. Expected actions are looked up from EXPECTATIONS
    based on the file basename.
    """
    basename = os.path.basename(file_path)
    expect = EXPECTATIONS[basename]

    with open(file_path, "rb") as file:
        file_content = file.read()

    os.remove(file_path)
    if expect["missing"] is None:
        test("Missing " + basename, expected_failure=True)
    else:
        test("Missing " + basename, expected_active=expect["missing"])

    if not os.path.exists(file_path):
        with open(file_path, "wb") as file:
            file.write(file_content)

    # Restore state of the project after a correct compilation
    Run([GPRBUILD, "-P", os.path.join("tree", "main.gpr"), "-p", "-j1", "-q"])

    with open(file_path, "a") as file:
        file.write("--  Comment that will not prevent a compilation for Ada files")

    test("Modified " + basename + " (comments only)",
         expected_active=expect["modified"])

    # Restore state of the project after a correct compilation
    Run([GPRBUILD, "-P", os.path.join("tree", "main.gpr"), "-p", "-j1", "-q"])


test("Build from scratch")

if "windows" in Env().host.platform:
    testsuite(os.path.join("tree", "obj", "main.exe"))
else:
    testsuite(os.path.join("tree", "obj", "main"))

testsuite(os.path.join("tree", "src", "main.adb"))
testsuite(os.path.join("tree", "src", "pkg.adb"))
testsuite(os.path.join("tree", "src", "pkg.ads"))
testsuite(os.path.join("tree", "src", "dep_two.adb"))
testsuite(os.path.join("tree", "src", "dep_two.ads"))
testsuite(os.path.join("tree", "obj", "b__main.ads"))
testsuite(os.path.join("tree", "obj", "b__main.adb"))
# Now, add content to source files that changes the code execution
file_path = os.path.join("tree", "src", "main.adb")

with open(file_path, "w") as file:
    file.write(
        """with Pkg;
function Main return Integer is
begin
   Pkg.P;
   return 1;
end Main;
"""
    )

test("Modified main.adb with different return code",
     expected_active={COMPILE_MAIN, BIND_MAIN, LINK_MAIN})

file_path = os.path.join("tree", "src", "pkg.adb")
with open(file_path, "r") as file:
    file_save = file.read()

with open(file_path, "w") as file:
    file.write(
        """
with Ada.Text_IO;

package body Pkg is

   procedure P is
   begin
      Ada.Text_IO.Put_Line ("Hello from Pkg");
   end P;
end Pkg;"""
    )

test("Modified pkg.adb without dep_two dependency",
     expected_active={COMPILE_PKG, BIND_MAIN, POST_BIND_MAIN, LINK_MAIN})

# Restore state of the project after a correct compilation
with open(file_path, "w") as file:
    file.write(file_save)
Run([GPRBUILD, "-P", os.path.join("tree", "main.gpr"), "-p"])

file_path = os.path.join("tree", "src", "dep_two.adb")
with open(file_path, "r") as file:
    file_save = file.read()

with open(file_path, "w") as file:
    file.write(
        """
with Ada.Text_IO;

package body Dep_Two is

   procedure P is
   begin
      Ada.Text_IO.Put_Line ("Hello from modified Dep_Two");
   end P;
end Dep_Two;"""
    )

test("Modified dep_two.adb (new output)",
     expected_active={COMPILE_DEP_TWO, BIND_MAIN, LINK_MAIN})

# Restore state of the project after a correct compilation
with open(file_path, "w") as file:
    file.write(file_save)
Run([GPRBUILD, "-P", os.path.join("tree", "main.gpr"), "-p"])
