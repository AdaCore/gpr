import re

from testsuite_support.builder_and_runner import BuilderAndRunner, GPRNAME, GPRLS, \
                                                 GPRINSTALL, GPRCLEAN, GPRCONFIG, \
                                                 GPRREMOTE

bnr = BuilderAndRunner()


def check(toolname, tool):
    try:
        p = BuilderAndRunner().run([tool, '--version'])
        if p.status != 0:
            print('gprname returned ' + str(p.status))
            print(p.out)
        else:
            output = p.out
            if not output.startswith(toolname + ' Pro ') or \
                    'This is free software; see the source for copying' not in output \
                    or re.search('.*Copyright \\(C\\) [0-9\\-]+, AdaCore.*',
                                 output) is None:
                print(toolname + ': ERROR')
                print(p.out)
            else:
                print(toolname + ': OK')

    except Exception as E:
        print('*** Error: %s' % str(E))


check('GPRNAME', GPRNAME)
check('GPRLS', GPRLS)
check('GPRINSTALL', GPRINSTALL)
check('GPRCLEAN', GPRCLEAN)
check('GPRCONFIG', GPRCONFIG)
check('GPRREMOTE', GPRREMOTE)
