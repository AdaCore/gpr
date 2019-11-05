from gnatpython.env import Env
from gnatpython.fileutils import cp
import os, subprocess

if Env().host.platform.endswith('windows'):
    exeext='.exe'
    cp('adactl', 'adactl' + exeext)
else:
    exeext=""

output=subprocess.check_output('gpr2install -p --prefix='
                               + os.getcwd() + '/inst inst.gpr', shell=True)

if os.path.exists('inst/share/doc/gps/html/main.html'):
    print("OK main html")
else:
    print("NOK")

if os.path.exists('inst/bin/adactl' + exeext):
    print("OK adactl")
else:
    print("NOK")
