import json
import os

from e3.env import Env
from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD

bnr = BuilderAndRunner()
bnr.call([GPRBUILD, "-q", "-P", os.path.join("tree", "app.gpr"),
          "-p", "--json-summary"])

with open(os.path.join("tree", "jobs.json")) as fp:
    jobs = json.load(fp)

links = [job for job in jobs if job["uid"].startswith("[Link] main")]
if len(links) == 1:
    command = links[0]["command"]
    print("encapsulated library linked:", "libbundle.a" in command)
    print("contained dependency linked again:", "libleaf.a" in command)

exe = ".exe" if "windows" in Env().host.platform else ""
result = bnr.simple_run(
    [os.path.join("tree", "main" + exe)], catch_error=True
)
print("result:", result.out.strip())
