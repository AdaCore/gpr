import subprocess

from testsuite_support.builder_and_runner import BuilderAndRunner, GPRNAME

bnr = BuilderAndRunner()


def run(args):
    print(bnr.check_output(args).out)


# very long source file support
try:
    run([GPRNAME, '-Pprj.gpr', '*.ad?'])
    subprocess.run("gprbuild -p -q -P prj.gpr", shell=True)
except Exception as E:
    # Unexpected exception.  Just print the information we have.
    print('*** Error: %s' % str(E))
