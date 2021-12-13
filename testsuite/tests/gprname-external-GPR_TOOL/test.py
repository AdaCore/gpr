import os
import subprocess

from testsuite_support.builder_and_runner import BuilderAndRunner, GPRNAME, GPRLS, \
                                                 GPRINSTALL, GPRCLEAN

bnr = BuilderAndRunner()


def run(args):
    print(bnr.check_output(args).out)


# set GPR_TOOL to gprbuild when gpr tools are parsing GPR files
try:
    if "GPRTOOL" in os.environ:
        os.environ.pop("GPR_TOOL")
    subprocess.run('gprbuild -p -q -P test.gpr', shell=True)
    run([GPRINSTALL, '-p', '-Ptest.gpr', '--prefix=./install'])
    run([GPRLS, '-Ptest.gpr'])
    run([GPRCLEAN, '-Ptest.gpr'])
    run([GPRNAME, '-Ptest.gpr', '-d', 'src', '*'])
except Exception as E:
    print('*** Error: %s' % str(E))
