from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD, GPRCLEAN

import distutils.ccompiler

bnr = BuilderAndRunner()
shared_lib_ext = distutils.ccompiler.new_compiler().shared_lib_extension

def run(cmd):
    print("$ " + " ".join(cmd))
    if cmd[0] in (GPRBUILD, GPRCLEAN):
        bnr.call(cmd)
    else:
        print(bnr.simple_run([cmd], catch_error=True).out)

def check(name):
    print(f"$ nm {name} | grep \"pkg\"")
    output = bnr.simple_run(["nm", "-j", name]).out
    symbols=[]
    for line in output.split("\n"):
        if "pkg" in line:
            symbols.append(line)
    symbols.sort()
    for symbol in symbols:
        print(symbol)

run([GPRBUILD, "-P", "static.gpr", "--src-subdirs=src2"])
check("lib/libstatic.a")

run([GPRBUILD, "-P", "esal.gpr", "--src-subdirs=src2", "-f"])
check(f"lib/libesal{shared_lib_ext}")

run([GPRBUILD, "-P", "agg_esal.gpr", "--src-subdirs=src2", "-f"])
check(f"agg_lib/libagg_esal{shared_lib_ext}")
