import subprocess

from testsuite_support.builder_and_runner import BuilderAndRunner, GPRNAME

bnr = BuilderAndRunner()


def run(args):
    print(bnr.check_output(args).out)


# check recursive dir support
try:
    run([GPRNAME, '-Pprj', '-d.', '-da/b', '-da/**', '-da/d', '*.spec',
         '*.body', '--ignore-duplicate-files'])
    subprocess.run("gprbuild -p -q -f -Pprj driver.body", shell=True)
    subprocess.run("./driver")
except Exception as E:
    print('*** Error: %s' % str(E))
