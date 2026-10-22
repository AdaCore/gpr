import json
import shlex
from pathlib import Path

from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD

bnr = BuilderAndRunner()
archive = Path("lib/libchecked.a")


def build(label, mode):
    print(label)
    bnr.check_call([GPRBUILD, "-Papp.gpr", "-p", "-q", "-j1",
                    "--json-summary", "-XARCHIVE_MODE=" + mode])
    jobs = json.loads(Path("jobs.json").read_text())
    for job in sorted(jobs, key=lambda job: job["uid"]):
        print(f"{job['uid']}: {job['status']}")
        if (job["uid"].startswith(("[Archive]", "[Partial Link]"))
                and job["status"] != "SKIPPED"):
            switches = shlex.split(job["command"])
            print("  --thin:", "--thin" in switches, "-T:", "-T" in switches)
    print("thin archive:", archive.read_bytes().startswith(b"!<thin>\n"))


def run():
    print("result:", bnr.simple_run(["app-obj/main"]).out.strip())


build("default options", "default")
run()
build("thin archive", "thin")
run()
build("unchanged build", "thin")

# Changing a member must relink even if the thin archive stays identical.
member_archive = Path("lib/libchecked.a")
before = member_archive.read_bytes()
source = Path("src/value.c")
source.write_text(source.read_text().replace("42", "43"))
build("member changed", "thin")
print("archive bytes unchanged:", member_archive.read_bytes() == before)
run()

build("legacy option", "legacy")
run()
build("empty options", "normal")
run()
