import os

from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD, GPRINSTALL

bnr = BuilderAndRunner()

prefix = os.path.join(os.getcwd(), "inst")

#  What "lkm install" does for libadalang: the same project installed once
#  per library kind, into a single prefix, each run naming its own build.

for kind in ("static", "static-pic", "relocatable"):
    bnr.check_output(
        [GPRBUILD, "-p", "-q", "-P", "lib.gpr", "-XLIBRARY_TYPE=" + kind]
    )

    print("=== install " + kind)
    print(
        bnr.run(
            [
                GPRINSTALL,
                "-p",
                "-P",
                "lib.gpr",
                "--prefix=" + prefix,
                "--build-var=LIBRARY_TYPE",
                "--build-name=" + kind,
                "-XLIBRARY_TYPE=" + kind,
            ]
        ).out
    )
