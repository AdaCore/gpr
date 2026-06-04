from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD
import os
import shutil
import json

bnr = BuilderAndRunner()

bnr.call([GPRBUILD, "-Papp.gpr"], quiet=True)

# Modify the mylib signature by adding a new procedure in mylib's pkg.ads.
# This change will not modify the produced .o used by importinglib library,
# because the new procedure is not used by importinglib.
# However, the ALI file of foo.ads will see its pkg.ads dependency change,
# so the copy of ALIs to the library directory must be done again.


shutil.copy(
    os.path.join("mylib", "src", "pkg.ads.new-version"),
    os.path.join("mylib", "src", "pkg.ads"),
)

p = bnr.call([GPRBUILD, "-Papp.gpr", "--json-summary"], quiet=True)
json_file = open("jobs.json")
jobs = json.load(json_file)

for job in jobs:
    if (
        "[Library-Files-Copy] ImportingLib (importinglib.gpr)" in job["uid"]
        and job["status"] == "0"
    ):
        print("OK")
        exit()

print("KO")
