import json
import shlex
from pathlib import Path

from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD

bnr = BuilderAndRunner()
archive = Path("obj/libapp.a")

for mode in ("default", "multiple", "legacy", "normal", "other"):
    print("mode:", mode)
    bnr.check_call([GPRBUILD, "-Papp.gpr", "-p", "-q", "-j1",
                    "--json-summary", "-XMODE=" + mode])
    jobs = json.loads(Path("jobs.json").read_text())
    for job in sorted(jobs, key=lambda job: job["uid"]):
        print(f"{job['uid']}: {job['status']}")
        if job["uid"].startswith("[Archive]"):
            command = shlex.split(job["command"])
            start = command.index("csrD") + 1
            end = next(i for i, arg in enumerate(command)
                       if Path(arg).name == "libapp.a")
            print("archive options:", command[start:end])
    print("thin archive:", archive.read_bytes().startswith(b"!<thin>\n"))
    print("result:", bnr.simple_run(["obj/main"]).out.strip())
