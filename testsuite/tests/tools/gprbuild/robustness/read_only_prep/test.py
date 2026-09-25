import os

from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD

bnr = BuilderAndRunner()
obj="obj"
prep = os.path.join(obj, "pkg.ads.prep")

# First build: integrated preprocessing (-gnateG) leaves the preprocessed
# source beside the object as "pkg.ads.prep".

bnr.run([GPRBUILD, "-p", "prj.gpr"])

print("preprocessed source after first build:", os.path.exists(prep))

# Make the obj dire read-only, so next forced build can not remove
# the pre source file and fails with the expected error message.

os.chmod(obj, 0o555)

try:
    bnr.call([GPRBUILD, "-f", "prj.gpr"])
finally:
    #  Always restore write permission, otherwise the working tree cannot be
    #  cleaned up after the test.
    os.chmod(obj, 0o755)

print("preprocessed source still present:", os.path.exists(prep))

# Check that build succeeds when the rights have been restored
bnr.call([GPRBUILD, "-f", "prj.gpr"])
