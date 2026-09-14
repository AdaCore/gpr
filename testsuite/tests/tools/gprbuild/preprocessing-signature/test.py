import json

from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD

bnr = BuilderAndRunner()


def build(label, proj):
    """Build, and report whether the Ada source was compiled again"""
    bnr.run([GPRBUILD, "-p", "-q", proj, "--json-summary"])

    with open("jobs.json") as fp:
        jobs = json.load(fp)

    for job in jobs:
        if job["uid"].startswith("[Ada Compile] pkg.ads"):
            print(
                label + ":",
                "skipped" if job["status"] == "SKIPPED" else "compiled",
            )


print("Absolute preprocessing file")
bnr.run([GPRBUILD, "-p", "-q", "prj_absolute.gpr"])
build("nothing changed", "prj_absolute.gpr")
with open("prep/prep.def") as fp:
    content = fp.read()
with open("prep/prep.def", "w") as fp:
    fp.write(content.replace("-DLEVEL=True", "-DLEVEL=False"))
build("data file changed", "prj_absolute.gpr")
build("nothing changed", "prj_absolute.gpr")

print("Simple named preprocessing file")
bnr.run([GPRBUILD, "-p", "-q", "prj_relative.gpr"])
build("nothing changed", "prj_relative.gpr")
with open("prep/prep.def") as fp:
    content = fp.read()
with open("prep/prep.def", "w") as fp:
    fp.write(content.replace("-DLEVEL=True", "-DLEVEL=False"))
build("data file changed", "prj_relative.gpr")
build("nothing changed", "prj_relative.gpr")
