from e3.env import Env
from testsuite_support.builder_and_runner import BuilderAndRunner

bnr = BuilderAndRunner()

if Env().host.platform.endswith('windows'):
    is_win = True
else:
    is_win = False

bnr.build('switches_attribute.gpr', args=['-p'])

bnr.run(['./main'], output="run.out")

for line in open("run.out"):
    li = line[:-1]
    # Windows has a non case-sensitive filesystem, we add there on
    # attribute to match the linux expected output.
    if is_win and li == 'A:   Switches [capital.adb] -> -g -gnata -g0':
        print('A:   Switches [Capital.adb] -> -g -gnata')
    print(li)
