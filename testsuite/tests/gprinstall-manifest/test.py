import os
import subprocess


subprocess.check_output('gprbuild -p -q simple.gpr', shell=True)
subprocess.check_output(
    'gpr2install -p --no-manifest --prefix={}/inst simple.gpr'
    .format(os.getcwd()),
    shell=True
)

if os.path.exists('inst/share/gpr/manifests/simple'):
    print("NOK")
else:
    print("OK")
