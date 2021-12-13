from testsuite_support.builder_and_runner import BuilderAndRunner, GPRNAME
from e3.diff import diff

bnr = BuilderAndRunner()


def run(args):
    print(bnr.check_output(args).out)


# preserve-case-sensitive-values
try:
    run([GPRNAME, '-Pprj.gpr', '*.ad?'])
    print(diff('expected2', 'prj.gpr'))
except Exception as E:
    print('*** Error: %s' % str(E))
