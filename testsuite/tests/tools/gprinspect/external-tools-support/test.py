import os
from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRINSPECT
from e3.env import Env
from e3.os.process import PIPE

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

def test (cmd, response=None):
    print(cmd)
    if response is None:
        bnr.run(cmd, output="gnatinspect.out")
    else:
        r = bnr.simple_run(
            cmd,
            env={"GPRINSPECT_TEST_OUTPUT": response},
            output="gnatinspect.out",
            error=PIPE,
        )
        # Check before filtering, which would hide warnings on stdout.
        with open("gnatinspect.out") as output:
            assert "Warning: Cannot import attribute registry" not in output.read()
    filter("gnatinspect.out")
    if response is not None:
        print(r.err, end="")

test([GPRINSPECT, '-Ptest', '--packages', '--attributes'])

env.add_search_path("PATH", os.getcwd())

bnr.build('test.gpr', args=['-p', '-q', '-XTEST_SRC=error'])
test([GPRINSPECT, '-Ptest', '--gpr-registry-file=gpr-registry-error.txt', '--packages', '--attributes'])

bnr.build('test.gpr', args=['-p', '-q', '-XTEST_SRC=invalid_json'])
test([GPRINSPECT, '-Ptest', '--gpr-registry-file=gpr-registry-invalid_json.txt', '--packages', '--attributes'])

bnr.build('test.gpr', args=['-p', '-q', '-XTEST_SRC=ok'])
test([GPRINSPECT, '-Ptest', '--gpr-registry-file=gpr-registry-ok.txt', '--packages', '--attributes'])

for response in ("valid", "empty", "invalid"):
    test(
        [GPRINSPECT, '-Ptest', '--packages', '--attributes'],
        response=response,
    )
