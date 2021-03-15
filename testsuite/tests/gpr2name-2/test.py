import os
import subprocess

from testsuite_support.builder_and_runner import BuilderAndRunner, GPRNAME

bnr = BuilderAndRunner()


def run(args):
    print(bnr.check_output(args).out)


# ** recursive dir switch support
try:
    os.chdir('recursive-dir')
    run([GPRNAME, '-Pprj', '-d.', '-da/b', '-da/**', '-da/d', '*.spec', '*.body',
         '--ignore-duplicate-files'])
    subprocess.check_output("gprbuild -p -q -f -P prj driver.body", shell=True)
    subprocess.run("./driver")
except Exception as E:
    # Unexpected exception.  Just print the information we have.
    print('*** Error: %s' % str(E))
finally:
    os.chdir('..')
