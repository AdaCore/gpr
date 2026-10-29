import json
from pathlib import Path

from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD

bnr = BuilderAndRunner()
archive = Path("obj/libapp.a")
member = Path("foreign-obj/member.o")
source = Path("foreign-src/member.c")


def build(label):
    print(label)
    bnr.check_call([GPRBUILD, "-Papp.gpr", "-p", "-q", "-j1", "--json-summary"])
    jobs = json.loads(Path("jobs.json").read_text())
    for job in sorted(jobs, key=lambda job: job["uid"]):
        print(f"{job['uid']}: {job['status']}")
        if job["uid"].startswith("[Link]") and job["status"] != "SKIPPED":
            # unused.o has an unresolved reference: expanding archive members
            # onto the link command would make linking fail.
            print("  member on command:", "member.o" in job["command"])
            print("  unused on command:", "unused.o" in job["command"])


def run():
    for main in ("main", "second"):
        print(main + ":", bnr.simple_run(["obj/" + main]).out.strip())


build("initial thin archive")
print("thin archive:", archive.read_bytes().startswith(b"!<thin>\n"))
run()
build("unchanged build")

# A skipped archive must still forward its members to a link that runs.
Path("obj/main").unlink()
build("missing executable")
run()
build("unchanged build after relinking")

before = archive.read_bytes()
size = member.stat().st_size
symbols = bnr.simple_run(["nm", "-g", "--defined-only", str(member)]).out
source.write_text(source.read_text().replace("return 1", "return 2"))
build("same-size member changed")
print("member size unchanged:", member.stat().st_size == size)
print("member symbols unchanged:", bnr.simple_run(
    ["nm", "-g", "--defined-only", str(member)]).out == symbols)
print("archive bytes unchanged:", archive.read_bytes() == before)
run()
