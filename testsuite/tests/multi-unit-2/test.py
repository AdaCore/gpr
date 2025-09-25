from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRLS, GPRBUILD, GPRCLEAN

import subprocess


bnr = BuilderAndRunner()

def build():
    bnr.check_call([GPRBUILD, "-p", "-q", "files/p.gpr"])

def clean():
    bnr.check_call([GPRCLEAN, "-p", "-q", "files/p.gpr"])

bnr.build("p.gpr", args=["-p"])

build()
bnr.check_call(["./main"])
clean()
bnr.check_call(["./main"])
build()
bnr.check_call([GPRLS, "-Pfiles/p.gpr", "few_units~2.ali"])
bnr.check_call([GPRLS, "-Pfiles/p.gpr", "few_units~4.ali"])
bnr.check_call([GPRLS, "-Pfiles/p.gpr", "few_units~6.ali"])
bnr.check_call([GPRLS, "-Pfiles/p.gpr", "few_units~7.ali"])
bnr.check_call([GPRLS, "-Pfiles/p.gpr", "pkg.ali"])
