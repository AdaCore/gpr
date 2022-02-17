import os
import subprocess

from testsuite_support.builder_and_runner import BuilderAndRunner, GPRINSTALL

bnr = BuilderAndRunner()

# build xyz
subprocess.check_output('gprbuild -p -q xyz/xyz.gpr', shell=True)

# install xyz library
bnr.check_output(
    [GPRINSTALL, '-p', '-r', '--prefix=' + os.getcwd() + '/inst',  'xyz/xyz.gpr'])

# build test checker
bnr.check_output(['gprbuild', '-p', '-q', '-Ptest.gpr'])

# run test checker
bnr.run(['./main'], output=None)
