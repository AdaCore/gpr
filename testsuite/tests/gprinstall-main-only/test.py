import os, subprocess

output=subprocess.check_output('gpr2install -p --prefix='
                               + os.getcwd() + '/inst inst.gpr', shell=True)

if os.path.exists('inst/share/doc/gps/html/main.html'):
    print("OK main html")
else:
    print("NOK")

if os.path.exists('inst/bin/adactl'):
    print("OK adactl")
else:
    print("NOK")
