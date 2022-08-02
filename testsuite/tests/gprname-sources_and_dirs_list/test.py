from testsuite_support.builder_and_runner import BuilderAndRunner, GPRNAME

bnr = BuilderAndRunner()


def run(args):
    print(bnr.check_output(args).out)


# check recursive dir support
try:
    res = bnr.check_output([GPRNAME, '-Dprj_source_list.txt',
                            '-Ptest.gpr', 'pack*.ad?'])

    for line in res.out.split("\n"):
        if "hidden by" in line:
            print(line)
        elif "pack2.ads" in line:
            print(line.replace("pack2.ads", "pack2.ad?"))
        elif "pack2.adb" in line:
            print(line.replace("pack2.adb", "pack2.ad?"))
    print()

    run(['gprbuild', '-Ptest.gpr'])
    run(['./main'])
except Exception as E:
    print('*** Error: %s' % str(E))
