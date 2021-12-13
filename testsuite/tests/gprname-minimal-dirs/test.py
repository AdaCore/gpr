import os
import subprocess

from testsuite_support.builder_and_runner import BuilderAndRunner, GPRNAME

bnr = BuilderAndRunner()


def run(args):
    print(bnr.check_output(args).out)


# ** recursive dir switch support
try:
    os.chdir('recursive-dir')
    run([GPRNAME, '-Pprj', '-d.', '-da/b', '-da/**', '-da/d', '-dempty',
         '*.spec', '*.body', '--ignore-duplicate-files', '--minimal-dirs'])
    with open("prj.gpr") as f:
        contents = f.read()
        if "**" in contents:
            print("** not expected in prj.gpr file")
        if "empty" in contents:
            print("empty not expected in prj.gpr file")
    subprocess.check_output("gprbuild -p -q -f -P prj driver.body", shell=True)
    subprocess.run("./driver")
except Exception as E:
    # Unexpected exception.  Just print the information we have.
    print('*** Error: %s' % str(E))
finally:
    os.chdir('..')
