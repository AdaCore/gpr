from testsuite_support.builder_and_runner import BuilderAndRunner, GPRNAME

bnr = BuilderAndRunner()


def run(args):
    print(bnr.check_output(args).out)


# check recursive dir support
try:
    run([GPRNAME, '-Dprj_source_list.txt', '-Ptest.gpr', 'pack*.ad?'])
    run(['gprbuild', '-Ptest.gpr'])
    run(['./main'])
except Exception as E:
    print('*** Error: %s' % str(E))
