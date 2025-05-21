from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD
import os

bnr = BuilderAndRunner()

bnr.simple_run([GPRBUILD, "-P" + os.path.join("tree", "lib.gpr"), "-p", "-q"])
bnr.build("test.gpr", args=['-p', '-q'])

# Call the link-options-add action and check that the new section has
# correctly been added.
bnr.call([os.path.join ("obj", "test")])
extracted_object = os.path.join ("obj", "pkg2.o")

if not os.path.exists(extracted_object):
    print("'" + extracted_object + "' does not exist")
else:
    output = bnr.simple_run(["nm", extracted_object]).out
    if "pkg2__p" in output and "pkg2_E" in output:
        print("Both 'pkg2__p' and 'pkg2_E' are present in the extracted object.")
    else:
        print("Either 'pkg2__p' or 'pkg2_E' is missing in the extracted object.")