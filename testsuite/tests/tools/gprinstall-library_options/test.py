import os
import subprocess

from testsuite_support.builder_and_runner import BuilderAndRunner, GPRINSTALL

bnr = BuilderAndRunner()

# build code.c & lib1 library
subprocess.check_output('gcc -c prj/code.c -o prj/code.o', shell=True)
subprocess.check_output('gprbuild -p -q prj/lib1.gpr', shell=True)

# install lib1 library
bnr.check_output(
    [GPRINSTALL, '-p', '--no-manifest',
     '--prefix=' + os.getcwd() + '/inst',  'prj/lib1.gpr'])

# build test checker
bnr.build('test.gpr')

# run test checker
bnr.run(['./main'], output=None)
