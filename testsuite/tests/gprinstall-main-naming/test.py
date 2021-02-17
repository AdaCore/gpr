import os
import subprocess

from testsuite_support.builder_and_runner import BuilderAndRunner, GPRINSTALL


subprocess.check_output('gprbuild -p -q main_naming.gpr', shell=True)

BuilderAndRunner().check_output(
    [GPRINSTALL, '-p', '--prefix=' + os.getcwd() + '/inst',
     'main_naming.gpr'])

if os.path.exists('inst/bin/main') or os.path.exists('inst/bin/main.exe'):
    print("OK main found")
else:
    print("NOK no main installed")
