import os
import subprocess

from testsuite_support.builder_and_runner import BuilderAndRunner, GPRNAME, GPRLS

bnr = BuilderAndRunner()


def run(args):
    print(bnr.check_output(args).out)


# check gprname's multi-unit support
try:
    os.system('cp prj.gpr_saved prj.gpr')
    run([GPRNAME, '-Pprj.gpr', '*.ada'])
    subprocess.run("gprbuild -p -q -P prj.gpr", shell=True)
    subprocess.run("./main")
    run([GPRLS, '-Pprj.gpr', '-U'])
except Exception as E:
    print('*** Error: %s' % str(E))
