import os

from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD, GPRCLEAN, GPRINSTALL

bnr = BuilderAndRunner()

prep = os.path.join("obj", "pkg.ads.prep")

bnr.run([GPRBUILD, "-p", "-q", "lib.gpr"])
bnr.run([GPRINSTALL, "--prefix=install", "-p", "lib.gpr"])

#  The installed spec must be the preprocessed one: no directive left, and
#  only the branch the symbol selected. Comments and blank lines are skipped,
#  as the preprocessor blanks out what it removes.

for line in open("install/include/lib/pkg.ads"):
    line = line.rstrip()

    if line and not line.lstrip().startswith("--"):
        print(line)

#  The preprocessed source is an output of the compilation, so gprclean
#  removes it along with the object and the ALI file.

print("preprocessed source after the build:", os.path.exists(prep))

bnr.run([GPRCLEAN, "-q", "lib.gpr"])

print("preprocessed source after gprclean:", os.path.exists(prep))

#  Build once more, then drop "-gnateG" and leave that file behind in the
#  object directory: nothing produces it any more, so the source itself must
#  be installed rather than the leftover.

bnr.run([GPRBUILD, "-p", "-q", "lib.gpr"])

with open("lib.gpr") as fp:
    content = fp.read()

with open("lib.gpr", "w") as fp:
    fp.write(content.replace(', "-gnateG"', ""))

bnr.run([GPRBUILD, "-p", "-q", "lib.gpr"])
bnr.run([GPRINSTALL, "--prefix=install-stale", "-p", "lib.gpr"])

print("preprocessed source left over:", os.path.exists(prep))

for line in open("install-stale/include/lib/pkg.ads"):
    line = line.rstrip()

    if line and not line.lstrip().startswith("--"):
        print(line)

#  gprclean removes it even then: the knowledge base lists ".prep" in
#  Clean'Source_Artifact_Extensions for Ada, which does not depend on
#  the switches of the build.

bnr.run([GPRCLEAN, "-q", "lib.gpr"])

print("left over after gprclean:", os.path.exists(prep))

#  The switch may also be given to gprbuild alone. gprinstall cannot see it
#  in the project then, so it relies on what the build recorded.

bnr.run([GPRBUILD, "-p", "-q", "lib.gpr", "-cargs:Ada", "-gnateG"])
bnr.run([GPRINSTALL, "--prefix=install-cargs", "-p", "lib.gpr"])

print("preprocessed source from -cargs:", os.path.exists(prep))

for line in open("install-cargs/include/lib/pkg.ads"):
    line = line.rstrip()

    if line and not line.lstrip().startswith("--"):
        print(line)
