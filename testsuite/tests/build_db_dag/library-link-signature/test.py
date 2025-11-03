from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD
import os
import shutil

bnr = BuilderAndRunner()

print("First compilation:")
bnr.call([GPRBUILD, "-Papp.gpr"])

# Modify the mylib signature by adding a new procedure in mylib's pkg.ads.
# This change will not modify the produced .o used by importinglib library,
# because the new procedure is not used by importinglib.
# However, the ALI file of foo.ads will see its pkg.ads dependency change,
# so the library-link signature of mylib will change.
# This is mandatory to ensure that the copy of the updated foo.ali file is made
# into the importinglib library directory, so that the build of app.gpr will
# not fail on gnatbind.
# So, the link signature of libraries must include the signatures of
# all its ALI files.

shutil.copy(
    os.path.join("mylib", "src", "pkg.ads.new-version"),
    os.path.join("mylib", "src", "pkg.ads"),
)
print("")
print("Second compilation:")
bnr.call([GPRBUILD, "-Papp.gpr"])
