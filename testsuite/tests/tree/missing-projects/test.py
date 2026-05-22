import os
import subprocess

from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD, GPRCLEAN

bnr = BuilderAndRunner()

bnr.build("missing_projects.gpr", args=["-p"])

def test(title):
    print(f"--- {title} ---")
    p = bnr.call(["./main"], quiet=True)
    for line in p.out.splitlines():
        # Prevent displaying search paths when a project file is not found
        if not line.startswith("   -"):
            print(line)
    print("")

print("without")
test("Without all environment variables set")

os.environ["ADA_PROJECT_PATH"] = "/non/existant/path"
os.environ["GPR_PROJECT_FILE_PATH"] = "project_files"
test("With all environment variables set")
