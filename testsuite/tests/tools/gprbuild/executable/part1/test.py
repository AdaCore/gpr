import json
import os.path
from e3.env import Env
from e3.os.process import Run

from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD

bnr = BuilderAndRunner()
test_number = 1

# Mapping from artifact basename to the job uid pattern expected to run
ARTIFACT_TO_JOB = {
    "main.o": {"[Ada Compile] main.adb"},
    "main.ali": {"[Ada Compile] main.adb"},
    "pkg.o": {"[Ada Compile] pkg.adb"},
    "pkg.ali": {"[Ada Compile] pkg.adb"},
    "dep_two.o": {"[Ada Compile] dep_two.adb"},
    "dep_two.ali": {"[Ada Compile] dep_two.adb"},
    "b__main.o": {"[Ada Post-Bind] b__main.adb"},
    "b__main.ali": {"[Ada Post-Bind] b__main.adb"},
    "main": {"[Link] main"},
    "main.exe": {"[Link] main"},
}


def test(header, expected_active=None):
    """Run gprbuild and check job statuses.

    expected_active: set of uid substrings for jobs expected to run (status '0').
                     If None, all jobs should have status '0' (build from scratch).
    """
    global test_number
    ok = True
    proc = bnr.call(
        [GPRBUILD, "-P", os.path.join("tree", "main.gpr"), "-p", "--json-summary", "-j1", "-q"])

    if proc.status:
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
    basename = os.path.basename(file_path)
    expected_active = ARTIFACT_TO_JOB.get(basename)

    with open(file_path, "rb") as file:
        file_content = file.read()

    os.remove(file_path)
    test("Missing " + basename, expected_active)

    if not os.path.exists(file_path):
        with open(file_path, "wb") as file:
            file.write(file_content)

    # Restore state of the project after a correct compilation
    Run([GPRBUILD, "-P", os.path.join("tree", "main.gpr"), "-p", "-j1", "-q"])

    with open(file_path, "a") as file:
        file.write("--  Comment that will not prevent a compilation for Ada files")

    test("Modified " + basename + " (comments only)", expected_active)

    # Restore state of the project after a correct compilation
    Run([GPRBUILD, "-P", os.path.join("tree", "main.gpr"), "-p", "-j1", "-q"])


test("Build from scratch")

testsuite(os.path.join("tree", "obj", "main.o"))
testsuite(os.path.join("tree", "obj", "main.ali"))
testsuite(os.path.join("tree", "obj", "pkg.o"))
testsuite(os.path.join("tree", "obj", "pkg.ali"))
testsuite(os.path.join("tree", "obj", "dep_two.o"))
testsuite(os.path.join("tree", "obj", "dep_two.ali"))
testsuite(os.path.join("tree", "obj", "b__main.o"))
testsuite(os.path.join("tree", "obj", "b__main.ali"))

if "windows" in Env().host.platform:
    exe=".exe"
else:
    exe=""

testsuite(os.path.join("tree", "obj", "main" + exe))
