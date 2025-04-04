import os
import json
from e3.fs import rm
from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPR2BUILD, GPR2CLEAN

bnr = BuilderAndRunner()

def run(cmd, cwd="", quiet=False):
    if cwd != "":
        old_cwd = os.getcwd()
        os.chdir(cwd)
    if not quiet:
        if isinstance (cmd, str):
            print("$ " + cmd);
        else:
            print("$ " + " ".join(cmd));
    if cmd[0] in (GPR2BUILD, GPR2CLEAN):
        bnr.call(cmd)
    else:
        print(bnr.simple_run([cmd], catch_error=True).out)
    if cwd != "":
        os.chdir(old_cwd)

def test(prj, switches, cleanup=True):
    run([GPR2BUILD, "-P", prj, "-q", "-p", "--json-summary", "-j1"] + switches)
    if os.path.exists("jobs.json"):
        with open("jobs.json") as fp:
            cnt = json.load(fp)
        rm("jobs.json")
        uids = dict((job["uid"], job) for job in cnt)

        for uid in sorted(uids):
            status = uids[uid]["status"]
            if status != "SKIPPED" and status != "DEACTIVATED":
                print(uid)

    if cleanup:
        run([GPR2CLEAN, "-P", prj, "-q", "-r"], quiet=True)


test("demo.gpr", ["-u"])
test("demo.gpr", ["-U"])

# check abstract project withing libs
test("abst.gpr", ["-u", "pkg2.ads"])
test("abst.gpr", ["-U", "pkg2.ads"])
test("abst.gpr", ["-U", "hello.c"])
test("abst.gpr", ["-U", "non_existing.ads"])
test("abst.gpr", ["-c", "pkg2.ads"])

# compile
test("demo.gpr", ["main.adb", "-c"], False)
# link intermediate libs
test("demo.gpr", ["main.adb", "-l"], False)
# bind
test("demo.gpr", ["main.adb", "-b"], False)
# link main
test("demo.gpr", ["main.adb", "-l"], False)
run(["./main"])
