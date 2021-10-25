from testsuite_support.builder_and_runner import BuilderAndRunner, GPRNAME
from e3.diff import diff

bnr = BuilderAndRunner()


def run(args):
    print(bnr.check_output(args).out)


# keep unknown attributes in generated GPR
try:
    run([GPRNAME, '-Pp.gpr', '*.adb'])
    print(diff('expected2', 'p.gpr'))
except Exception as E:
    # Unexpected exception.  Just print the information we have.
    print('*** Error: %s' % str(E))
