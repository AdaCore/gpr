import os

from testsuite_support.builder_and_runner import BuilderAndRunner, GPRNAME

bnr = BuilderAndRunner()


def run(args):
    print(bnr.check_output(args).out)


# check saved files created as expected
try:
    os.system('rm -f *saved*')
    os.system('rm -f prj_*')
    os.system('cp prj.gpr_orig prj.gpr')
    run([GPRNAME, '-Pprj.gpr', '*.ad?'])
    os.system('ls -1 *saved*')
    run([GPRNAME, '-Pprj.gpr', '*.ad?', '--no-backup'])
    os.system('ls -1 *saved*')
    run([GPRNAME, '-Pprj.gpr', '*.ad?'])
    os.system('ls -1 *saved*')
except Exception as E:
    # Unexpected exception.  Just print the information we have.
    print('*** Error: %s' % str(E))
