import os, subprocess

output=subprocess.check_output('gprbuild -p -q main_naming.gpr', shell=True)

output=subprocess.check_output('gpr2install -p --prefix='
                               + os.getcwd() + '/inst main_naming.gpr',
                               shell=True)

if os.path.exists('inst/bin/main') or os.path.exists('inst/bin/main.exe'):
    print("OK main found")
else:
    print("NOK no main installed")
