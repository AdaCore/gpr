import os

from e3.os.process import Run

os.mkdir('obj')

# first run without .ali (no compilation)
Run(['gpr2ls', '-d', 'multi.gpr'], output='run.out')

print('No build')
for line in open("run.out"):
    print(line[:-1])

Run(['gprbuild', '-p', '-q', 'multi.gpr'])

# second run with .ali (after compilation)
Run(['gpr2ls', '-d', 'multi.gpr'], output='run.out')

print('With build')
for line in open("run.out"):
    print(line[:-1])
