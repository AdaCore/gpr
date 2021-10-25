import os

from testsuite_support.builder_and_runner import BuilderAndRunner, GPRNAME
bnr = BuilderAndRunner()


def run(args):
    print(bnr.check_output(args).out)


# check extending project support
try:
    run([GPRNAME, '-Pa.gpr', '*.ada'])
    os.system('cat a.gpr')
except Exception as E:
    print('*** Error: %s' % str(E))
