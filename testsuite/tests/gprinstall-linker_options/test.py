import os
import subprocess

from testsuite_support.builder_and_runner import BuilderAndRunner, GPRINSTALL

bnr = BuilderAndRunner()

# build lib1 & lib2 library
subprocess.check_output('gprbuild -p -q prj/lib1.gpr', shell=True)
subprocess.check_output('gprbuild -p -q prj/lib2.gpr', shell=True)

# install lib2 library
bnr.check_output(
    [GPRINSTALL, '-p', '--no-manifest',
     '--prefix=' + os.getcwd() + '/inst',  'prj/lib2.gpr'])

# build test checker
bnr.build('test.gpr')

# run test checker
bnr.run(['./main'], output=None)
