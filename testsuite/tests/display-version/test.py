import re

from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRLS, GPRINSTALL, GPRCONFIG, GPRREMOTE, GPRCLEAN

bnr = BuilderAndRunner()


def check(toolname, tool):
    try:
        p = BuilderAndRunner().run([tool, '--version'])
        if p.status != 0:
            print(toolname + ' returned ' + str(p.status))
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


check('GPRLS', GPRLS)
check('GPRINSTALL', GPRINSTALL)
check('GPRCLEAN', GPRCLEAN)
check('GPRCONFIG', GPRCONFIG)
check('GPRREMOTE', GPRREMOTE)
