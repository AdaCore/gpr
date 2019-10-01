import os, subprocess

output=subprocess.check_output('gprbuild -p -q simple.gpr', shell=True)

output=subprocess.check_output('gpr2install -p --no-manifest --prefix='
                               + os.getcwd() + '/inst simple', shell=True)

if os.path.exists('inst/share/gpr/manifests/simple'):
    print("NOK")
else:
    print("OK")
