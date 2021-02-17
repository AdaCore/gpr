import os

from e3.env import Env
from e3.fs import cp
from testsuite_support.builder_and_runner import BuilderAndRunner, GPRINSTALL

if Env().host.platform.endswith('windows'):
    exeext = '.exe'
    cp('adactl', 'adactl' + exeext)
else:
    exeext = ""

p = BuilderAndRunner().run([GPRINSTALL, '-p',
                            '--prefix=' + os.path.join(os.getcwd(), 'inst'),
                            'inst.gpr'])

if os.path.exists('inst/share/doc/gps/html/main.html'):
    print("OK main html")
else:
    print("NOK")

if os.path.exists('inst/bin/adactl' + exeext):
    print("OK adactl")
else:
    print("NOK")
