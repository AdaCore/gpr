from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD
import os

bnr = BuilderAndRunner()

bnr.simple_run([GPRBUILD, "-P" + os.path.join("tree", "lib.gpr"), "-p", "-q", "-c"])
bnr.build("test.gpr", args=['-p', '-q'])

# Call the link-options-insert action and check that the new section has
# correctly been added.
bnr.call([os.path.join ("obj", "test")])
output = bnr.call(["objdump", "-s", os.path.join("tree", "obj", "pkg-with-linker-options.o"), "--section=.GPR.linker_options"], quiet=True).out
expected_output_lines = [
    "Contents of section .GPR.linker_options:",
    "Option1.--Option",
    "2.-o.--Option1=h",
    "ello.option with",
    "whitespaces"
]

success = True
for expected in expected_output_lines:
    if expected not in output:
        print(f"Expected '{expected}' not found in output data.")
        success=False
        break

if not success:
    print("Obtained output:")
    print(output)
else:
    print("Success")