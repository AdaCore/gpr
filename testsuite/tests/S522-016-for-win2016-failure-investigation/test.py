import subprocess

output=subprocess.check_output('gprbuild -v -p -Pgauges', shell=True)
print output
output=subprocess.check_output('gpr2ls -P gauges --closure gauge', shell=True)
print output
output=subprocess.check_output('ls -laR --full-time', shell=True)
print output
output=subprocess.check_output('gpr2clean -p -v -r gauges.gpr', shell=True)
print output
output=subprocess.check_output('ls -laR --full-time', shell=True)
print output
