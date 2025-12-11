import json
from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD
from e3.env import Env

bnr = BuilderAndRunner()


def run(cmd):
    print("$ " + " ".join(cmd))
    if cmd[0] == GPRBUILD:
        bnr.call(cmd)
    else:
        print(bnr.simple_run([cmd], catch_error=True).out)


run([GPRBUILD, "-q", "-Pmylib.gpr", "-p", "--json-summary", "-j1"])
with open("jobs.json") as fp:
    cntlib = json.load(fp)

lib_version_found = False
lib_version_switches_found = False
for job in cntlib:
    if job["status"] != "SKIPPED":
        if "-Wl,-soname,libmylib.1.2.3.so" in job["command"]:
            lib_version_switches_found = True
        if "lib/libmylib.1.2.3.so" in job["command"]:
            lib_version_found = True

if 'windows' in Env().host.platform:
    if lib_version_found or lib_version_switches_found:
        print(
            "ERROR: on windows, library versioning should not have an impact on windows"
        )
    else:
        print("SUCCESS")
else:
    if lib_version_found and lib_version_switches_found:
        print("SUCCESS")
    else:
        print("ERROR: library versioning not applied correctly")
