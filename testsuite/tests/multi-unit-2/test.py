from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRLS, GPRBUILD

import subprocess


bnr = BuilderAndRunner()

def build():
    bnr.check_call([GPRBUILD, "-p", "-q", "files/p.gpr"])

build()
bnr.build("p.gpr", args=["-p"])
bnr.check_call(["./main"])
build()
bnr.check_call([GPRLS, "-Pfiles/p.gpr", "few_units~2.ali"])
bnr.check_call([GPRLS, "-Pfiles/p.gpr", "few_units~4.ali"])
bnr.check_call([GPRLS, "-Pfiles/p.gpr", "few_units~6.ali"])
bnr.check_call([GPRLS, "-Pfiles/p.gpr", "few_units~7.ali"])
bnr.check_call([GPRLS, "-Pfiles/p.gpr", "pkg.ali"])
