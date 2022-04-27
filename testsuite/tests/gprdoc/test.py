import os
import subprocess

from testsuite_support.builder_and_runner import GPRDOC

try:
    subprocess.run([GPRDOC, "--display=json"])
except Exception as E:
    # Unexpected exception.  Just print the information we have.
    print('*** Error: %s' % str(E))
finally:
    os.chdir('..')
