import os

from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD

bnr = BuilderAndRunner()


def run(cmd):
    print("$ " + " ".join(cmd))
    bnr.call(cmd)


run([GPRBUILD, "-P", "lib.gpr", "-p", "-q"])

# Nothing changed since the first build, so this one must execute no action
# at all: it is not quiet, so any action it runs shows up in the output.
run([GPRBUILD, "-P", "lib.gpr"])

# The copy of a non-Ada interface source is an output of the library files
# copy like any other, so removing it must invalidate the signature.
print("$ rm libsrc/cutil.h")
os.remove(os.path.join("libsrc", "cutil.h"))

run([GPRBUILD, "-P", "lib.gpr"])

print("$ ls libsrc")
for Name in sorted(os.listdir("libsrc")):
    print(Name)
