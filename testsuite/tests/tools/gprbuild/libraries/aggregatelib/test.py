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

for kind in "static", "relocatable":
    print(f"*** test with {kind} aggregate lib ***")
    lib_kind = "-XAGG_LIBRARY_KIND=" + kind
    run([GPRBUILD, "-q", "-Pagglib.gpr", "-p", "--json-summary", lib_kind])
    with open("jobs.json") as fp:
        cnt = json.load(fp)
    jobs = {job["uid"]: job["command"] for job in cnt}
    for uid in sorted(jobs.keys()):
        print(uid)
        if "-fpic" in jobs[uid].lower():
            print("uses fpic")

    run([GPRBUILD, "-q", "-Pdemo.gpr", "-p", "--json-summary", lib_kind])
    with open("jobs.json") as fp:
        cnt = json.load(fp)
    for job in cnt:
        if "[Link]" in job["uid"] and "-o main" in job["command"]:
            if "libdemo.a" in job["command"]:
                print("Ok: linking with libdemo.a")
            if "-ldemo" in job["command"]:
                print("Ok: linking with libdemo")
            if "pkg1.o" in job["command"]:
                print("ERROR: linking with pkg1.o")
            elif "pkg2.o" in job["command"]:
                print("ERROR: linking with pkg2.o")
            else:
                print("Ok: not linking with individual .o's")
run(["./main"])
