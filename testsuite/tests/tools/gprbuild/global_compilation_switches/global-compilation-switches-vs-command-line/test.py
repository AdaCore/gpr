import json

from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD

bnr = BuilderAndRunner()

# -gnatwe comes from Builder'Global_Compilation_Switches (project source) and
# -gnatwn is passed on the command line. GNAT warning switches are "last one
# wins", so the command-line switch must appear after the project one on the
# generated compiler command line to take precedence, matching legacy
# gprbuild's behavior.
#
# main.adb is warning-free, so the build succeeds regardless of the switch
# order; this test only checks the ordering of the two switches on the
# compile command line.
bnr.simple_run(
    [GPRBUILD, "-P", "prj.gpr", "-p", "-q", "--json-summary", "-j1", "-gnatwn"]
)

with open("jobs.json") as f:
    jobs = json.load(f)

compile_cmd = None
for job in jobs:
    cmd = job.get("command", "")
    if "main.adb" in cmd and "-gnatwe" in cmd:
        compile_cmd = cmd
        break

if compile_cmd is None:
    print("ERROR: could not find the main.adb compile command carrying -gnatwe")
elif "-gnatwn" not in compile_cmd:
    print("ERROR: the command-line switch -gnatwn is missing from the "
          "compile command")
elif compile_cmd.rfind("-gnatwn") > compile_cmd.rfind("-gnatwe"):
    print("Command-line switch overrides the project switch")
else:
    print("ERROR: the project switch -gnatwe appears after the command-line "
          "switch -gnatwn")
