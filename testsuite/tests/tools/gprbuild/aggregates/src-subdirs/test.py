from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD, GPRCLEAN

import distutils.ccompiler
import os

bnr = BuilderAndRunner()
shared_lib_ext = distutils.ccompiler.new_compiler().shared_lib_extension

def run(cmd):
    print("$ " + " ".join(cmd))
    if cmd[0] in (GPRBUILD, GPRCLEAN):
        bnr.call(cmd)
    else:
        print(bnr.simple_run([cmd], catch_error=True).out)

def check(name, expected):
    print(f"$ nm {os.path.splitext(name)[0]} | grep \"pkg\"")
    output = bnr.simple_run(["nm", "-j", name]).out

    missing = [s for s in expected if s not in output]

    if missing:
        print(f"Missing symbol : {', '.join(missing)}")
    else:
        if len(expected) == 1:
            print(f"{expected[0]} symbol was found.")
        else:
            print(f"{', '.join(expected)} symbols were found.")

run([GPRBUILD, "-P", "static.gpr", "--src-subdirs=src2", "-q"])
check("lib/libstatic.a", ["pkg_E"])

run([GPRBUILD, "-P", "esal.gpr", "--src-subdirs=src2", "-f", "-q"])
check(f"lib/libesal{shared_lib_ext}", ["pkg_E", "pkgS"])

run([GPRBUILD, "-P", "agg_esal.gpr", "--src-subdirs=src2", "-f", "-q"])
check(f"agg_lib/libagg_esal{shared_lib_ext}", ["pkg_E", "pkgS"])
