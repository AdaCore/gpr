from testsuite_support.builder_and_runner import BuilderAndRunner, GPRNAME

bnr = BuilderAndRunner()


def run(args):
    print(bnr.check_output(args).out)


# allow -P with path
try:
    run([GPRNAME, '-PA/p.gpr', "-d", "src", '*'])
except Exception as E:
    print('*** Error: %s' % str(E))
