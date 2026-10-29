import json
from pathlib import Path

from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD

bnr = BuilderAndRunner()


def build(label):
    print(label)
    bnr.check_call([GPRBUILD, "-Papp.gpr", "-p", "-q", "-j1", "--json-summary"])
    for job in sorted(json.loads(Path("jobs.json").read_text()),
                      key=lambda job: job["uid"]):
        print(f"{job['uid']}: {job['status']}")
    print("result:", bnr.simple_run(["obj/main"]).out.strip())


build("initial thin archive")
extra = Path("foreign-src/extra.c")
extra.write_text("int extra(void) { return 3; }\n")
build("member added")
extra.unlink()
build("member removed")

member = Path("foreign-obj/member.o")
member.unlink()
build("missing referenced object")
print("member regenerated:", member.exists())
