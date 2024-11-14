import os
from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRINSPECT
from e3.env import Env

env = Env()
bnr = BuilderAndRunner()

def filter(file):
    source_ok_found = False
    with open(file) as F:
        for line in F:
            if source_ok_found:
                print(line.rstrip())
            if ' - "src_ok"' in line:
                source_ok_found = True

def test (cmd):
    print(cmd)
    r = bnr.run(cmd, output="gnatinspect.out")
    filter("gnatinspect.out")

test([GPRINSPECT, '-Ptest', '--packages', '--attributes'])

env.add_search_path("PATH", os.getcwd())

bnr.build('test.gpr', args=['-p', '-q', '-XTEST_SRC=error'])
test([GPRINSPECT, '-Ptest', '--gpr-registry-file=gpr-registry-error.txt', '--packages', '--attributes'])

bnr.build('test.gpr', args=['-p', '-q', '-XTEST_SRC=invalid_json'])
test([GPRINSPECT, '-Ptest', '--gpr-registry-file=gpr-registry-invalid_json.txt', '--packages', '--attributes'])

bnr.build('test.gpr', args=['-p', '-q', '-XTEST_SRC=ok'])
test([GPRINSPECT, '-Ptest', '--gpr-registry-file=gpr-registry-ok.txt', '--packages', '--attributes'])
