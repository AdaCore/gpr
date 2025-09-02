import os
import subprocess

from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD, GPRINSTALL

bnr = BuilderAndRunner()

# build lib1 & lib2 library
bnr.check_output([GPRBUILD, '-p', '-q', 'prj/lib1.gpr'])
bnr.check_output([GPRBUILD, '-p', '-q', 'prj/lib2.gpr'])

# install lib2 library
bnr.check_output(
    [GPRINSTALL, '-p', '--no-manifest',
     '--prefix=' + os.getcwd() + '/inst',  'prj/lib2.gpr'])

# build test checker
bnr.build('test.gpr')

# run test checker
bnr.run(['./main'], output=None)
