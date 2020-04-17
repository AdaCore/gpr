import subprocess

for args in [
    'gprbuild -v -p -Pgauges',
    'gpr2ls -P gauges --closure gauge',
    'ls -laR --full-time',
    'gpr2clean -p -v -r gauges.gpr',
    'ls -laR --full-time',
]:
    print(subprocess.check_output(args, shell=True))
