from testsuite_support.builder_and_runner import BuilderAndRunner, GPRNAME
from e3.diff import diff

bnr = BuilderAndRunner()


def run(args):
    print(bnr.check_output(args).out)


# limit generated gpr changes
try:
    run([GPRNAME, '-Pp.gpr', '-d', 'src', '*.ads', '-f*.c'])
    print(diff('naming_expected2', 'p_naming.gpr'))
    print(diff('source_list_expected', 'p_source_list.txt'))
except Exception as E:
    print('*** Error: %s' % str(E))
