from testsuite_support.builder_and_runner import BuilderAndRunner, GPRNAME

bnr = BuilderAndRunner()


def run(args):
    p = bnr.run(args)
    if p.status != 0:
        print('gprname returned ' + str(p.status))
        print(p.out)


# invalid gpr handling test
try:
    run([GPRNAME, '-Pprj', '*.body'])
except Exception as E:
    print('*** Error: %s' % str(E))
