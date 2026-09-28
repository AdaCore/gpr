import os

from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD

bnr = BuilderAndRunner()


def run(cmd):
    print("$ " + " ".join(cmd))
    if cmd[0] == GPRBUILD:
        bnr.call(cmd)
    else:
        print(bnr.simple_run([cmd], catch_error=True).out)


# Let windows find the dynamic lib
os.environ["PATH"] = os.pathsep.join(
    [os.path.join(os.getcwd(), "derived", "lib"), os.environ["PATH"]]
)

# Build the extended library on its own first, so that its ALI files exist
# and can be inherited by the extending library.
run([GPRBUILD, "-q", "-Pbase/base.gpr", "-p"])

# Defs is the only unit the extending library redefines. Making it the most
# recent source forces Printer and Extra to be recompiled there: the ALIs to
# copy are then the ones of the extending library.
os.utime(os.path.join("derived", "src", "defs.ads"), None)

run([GPRBUILD, "-q", "-Papp/app.gpr", "-p"])
run([os.path.join(".", "app", "exec", "main")])
