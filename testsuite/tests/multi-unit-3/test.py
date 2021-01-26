import os
import subprocess

from e3.os.process import Run

os.mkdir('obj')

# first run without .ali (no compilation)
r = subprocess.getoutput('gpr2ls -d -P multi.gpr')

print('No build')
print(r)

Run(['gprbuild', '-p', '-q', 'multi.gpr'])

# second run with .ali (after compilation)
r = subprocess.getoutput('gpr2ls -d -P multi.gpr')

print('With build')
print(r)
