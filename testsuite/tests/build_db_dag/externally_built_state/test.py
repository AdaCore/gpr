import os

from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD

bnr = BuilderAndRunner()

# build the library while it is still not externally built, so that the
# externally built view has actual artifacts to work with
bnr.call([GPRBUILD, "-Ptree/ext/ext.gpr", "-p", "-q", "-XEXT_BUILT=false"])

# main.gpr is only loaded, never built: create its object directory so that
# loading it does not warn
os.makedirs(os.path.join("tree", "obj"), exist_ok=True)

bnr.build(project="test.gpr", args=["-p", "-q"])
bnr.call(["./main"])
