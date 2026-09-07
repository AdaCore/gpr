import os

from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD

bnr = BuilderAndRunner()

restrictions = os.path.abspath("tree/restrictions.adc")

print(f"$ gprbuild -c -P tree/prj.gpr -gnatec={restrictions}")
bnr.check_call(
    [GPRBUILD, "-c", f"-gnatec={restrictions}", "-P", "tree/prj.gpr"]
)

print(f"$ gprbuild -c -P tree/prj.gpr -gnatec={restrictions}")
bnr.check_call(
    [GPRBUILD, "-c", f"-gnatec={restrictions}", "-P", "tree/prj.gpr"]
)

print("$ editing restrictions.adc: now restricts No_Calendar instead")
with open("tree/restrictions.adc", "w") as f:
    f.write(
        'pragma Warnings (Off, "[enabled by default]");\n'
        "pragma Restriction_Warnings (No_Calendar);\n"
    )

print(f"$ gprbuild -c -P tree/prj.gpr -gnatec={restrictions}")
bnr.check_call(
    [GPRBUILD, "-c", f"-gnatec={restrictions}", "-P", "tree/prj.gpr"]
)

print(f"$ gprbuild -c -P tree/prj.gpr -gnatec={restrictions}")
bnr.check_call(
    [GPRBUILD, "-c", f"-gnatec={restrictions}", "-P", "tree/prj.gpr"]
)
