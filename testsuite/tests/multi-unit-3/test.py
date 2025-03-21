import os

from e3.os.process import Run
from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRLS, GPRBUILD

bnr = BuilderAndRunner()
os.mkdir('obj')

# first run without .ali (no compilation)
r = bnr.check_output([GPRLS, '-d', '-Pmulti'])

print('No build')
print(r.out)

Run([GPRBUILD, '-p', '-q', 'multi.gpr'])

# second run with .ali (after compilation)
r = bnr.check_output([GPRLS, '-d', '-Pmulti'])

print('With build')
print(r.out)
