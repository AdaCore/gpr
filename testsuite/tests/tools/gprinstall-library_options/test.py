import os
import subprocess

from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD, GPRINSTALL

bnr = BuilderAndRunner()

# build code.c & lib1 library
subprocess.check_output('gcc -c prj/code.c -o prj/code.o', shell=True)
bnr.check_output([GPRBUILD, '-p', '-q', 'prj/lib1.gpr'])

# install lib1 library
bnr.check_output(
    [GPRINSTALL, '-p', '--no-manifest',
     '--prefix=' + os.getcwd() + '/inst',  'prj/lib1.gpr'])

# build test checker
bnr.build('test.gpr')

# run test checker
bnr.run(['./main'], output=None)
