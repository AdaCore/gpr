import json
import os

from e3.env import Env
from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD

bnr = BuilderAndRunner()

exe = ".exe" if "windows" in Env().host.platform else ""
prj = os.path.join("tree", "hello_from_ada.gpr")

#  The executable's only direct dependency is the Ada library. The Rust one
#  is reached through it, so nothing in the executable's own inputs mentions
#  Rust.
bnr.call([GPRBUILD, "-q", "-P", prj, "-p", "--json-summary"])

print(bnr.simple_run(
    [os.path.join("tree", "hello_from_ada" + exe)], catch_error=True).out)

#  The switches a Rust library needs must still reach the final link. On a
#  current Linux -pthread happens to be a no-op, so running the binary proves
#  nothing: look at the command instead. It must be the one that produces the
#  executable, as an intermediate command carrying the switch would say
#  nothing about the link that needs it.
with open(os.path.join("tree", "jobs.json")) as fp:
    jobs = json.load(fp)

wanted = "-lws2_32" if "windows" in Env().host.platform else "-pthread"

links = [job for job in jobs if job["uid"].startswith("[Link] hello_from_ada")]

print("links of hello_from_ada:", len(links))

for job in links:
    print(
        "the Rust link switches reached it:",
        wanted in job["command"].split(),
    )
