import os

from testsuite_support.builder_and_runner import BuilderAndRunner, GPRNAME

bnr = BuilderAndRunner()


def run(args):
    p = bnr.run(args)
    if p.status != 0:
        print('gprname returned ' + str(p.status))
        print(p.out)


# check invalid project not changed by gprname
try:
    os.system('cp prj.gpr_good prj.gpr')
    run([GPRNAME, '-Pprj', '*.body'])
    os.system('cat prj.gpr')
    os.system('cp prj.gpr_bad prj.gpr')
    run([GPRNAME, '-Pprj', '*.body'])
    os.system('cat prj.gpr')
except Exception as E:
    print('*** Error: %s' % str(E))
