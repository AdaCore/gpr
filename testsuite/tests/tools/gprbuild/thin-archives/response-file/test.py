import json
from pathlib import Path

from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD

bnr = BuilderAndRunner()
archive = Path("obj/libapp.a")

# Switching archive modes forces execution: response-file selection alone
# does not affect the build signature.
for mode in ("normal", "thin"):
    print("mode:", mode)
    bnr.check_call([GPRBUILD, "-Papp.gpr", "-p", "-q", "-j1",
                    "--json-summary", "-XMODE=" + mode])
    for job in sorted(json.loads(Path("jobs.json").read_text()),
                      key=lambda job: job["uid"]):
        print(f"{job['uid']}: {job['status']}")
        if job["uid"].startswith("[Archive]"):
            print("  thin option:", "--thin" in job["command"])
            print("  response file:", "@" in job["command"])
    print("thin archive:", archive.read_bytes().startswith(b"!<thin>\n"))
    print("result:", bnr.simple_run(["obj/main"]).out.strip())
