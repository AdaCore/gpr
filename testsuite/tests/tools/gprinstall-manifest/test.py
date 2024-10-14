import os
import subprocess

from testsuite_support.builder_and_runner import BuilderAndRunner, GPRINSTALL, GPRBUILD

bnr = BuilderAndRunner()

bnr.check_output([GPRBUILD, '-p', '-q', 'simple.gpr'])
bnr.check_output([GPRINSTALL, '-p', '--no-manifest', '--prefix=' + os.getcwd() + '/inst',  'simple.gpr'])

if os.path.exists('inst/share/gpr/manifests/simple'):
    print("NOK")
else:
    print("OK")
