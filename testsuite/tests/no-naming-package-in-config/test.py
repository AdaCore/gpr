import subprocess

output=subprocess.check_output('gpr2clean -p -q p.gpr --config=p.cgpr', shell=True)
print output
