import subprocess

from testsuite_support.builder_and_runner import BuilderAndRunner, GPRNAME
from e3.diff import diff

bnr = BuilderAndRunner()


def run(args):
    print(bnr.check_output(args).out)


# gpr2name -P switch support
try:
    run([GPRNAME, '-Pprj.gpr', '*.ada'])
    print(diff('prj.expected', 'prj.gpr'))
    subprocess.run("gprbuild -p -q -P prj.gpr main.2.ada -o main", shell=True)
    subprocess.run("./main")
except Exception as E:
    # Unexpected exception.  Just print the information we have.
    print('*** Error: %s' % str(E))
