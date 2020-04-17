import os

from e3.env import Env
from e3.fs import cp
from e3.os.process import Run

if Env().host.platform.endswith('windows'):
    exeext = '.exe'
    cp('adactl', 'adactl' + exeext)
else:
    exeext = ""

p = Run(['gpr2install', '-p',
         '--prefix=' + os.path.join(os.getcwd(), 'inst'), 'inst.gpr'])

if os.path.exists('inst/share/doc/gps/html/main.html'):
    print("OK main html")
else:
    print("NOK")

if os.path.exists('inst/bin/adactl' + exeext):
    print("OK adactl")
else:
    print("NOK")
