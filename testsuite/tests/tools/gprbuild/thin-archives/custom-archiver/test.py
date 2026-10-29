import json
import sys
from pathlib import Path

from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD

bnr = BuilderAndRunner()
archive = Path("obj/libapp.a")


def build(label):
    print(label)
    bnr.check_call([GPRBUILD, "-Papp.gpr", "-p", "-q", "-j1", "--json-summary",
                    "-XPYTHON=" + sys.executable])
    for job in sorted(json.loads(Path("jobs.json").read_text()),
                      key=lambda job: job["uid"]):
        print(f"{job['uid']}: {job['status']}")
    print("result:", bnr.simple_run(["obj/main"]).out.strip())


build("custom options")
print("thin archive:", archive.read_bytes().startswith(b"!<thin>\n"))
build("unchanged build")

# Dependency tracking must work without recognizing the archiver's switches.
before = archive.read_bytes()
source = Path("foreign-src/member.c")
source.write_text(source.read_text().replace("return 1", "return 2"))
build("member changed")
print("archive bytes unchanged:", archive.read_bytes() == before)
