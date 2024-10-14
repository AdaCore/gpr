import os
import subprocess

from testsuite_support.builder_and_runner import BuilderAndRunner, GPRINSTALL, GPRBUILD

bnr = BuilderAndRunner()

# build xyz
bnr.check_output([GPRBUILD, '-p', '-q', '-Pxyz/xyz.gpr']

# install xyz library
bnr.check_output(
    [GPRINSTALL, '-p', '-r', '--prefix=' + os.getcwd() + '/inst',  'xyz/xyz.gpr'])

# build test checker
bnr.check_output([GPRBUILD, '-p', '-q', '-Ptest.gpr'])

# run test checker
bnr.run(['./main'], output=None)
