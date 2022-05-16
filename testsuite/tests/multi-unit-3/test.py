import os

from e3.os.process import Run
from testsuite_support.builder_and_runner import BuilderAndRunner, GPRLS

bnr = BuilderAndRunner()
os.mkdir('obj')

# first run without .ali (no compilation)
r = bnr.check_output([GPRLS, '-d', '-Pmulti'])

print('No build')
print(r.out)

Run(['gprbuild', '-p', '-q', 'multi.gpr'])

# second run with .ali (after compilation)
r = bnr.check_output([GPRLS, '-d', '-Pmulti'])

print('With build')
print(r.out)
