import subprocess

from testsuite_support.builder_and_runner import BuilderAndRunner, GPRNAME

bnr = BuilderAndRunner()


def run(args):
    print(bnr.check_output(args).out)


# ignore-duplicate-files switch support
try:
    run([GPRNAME, '-Pprj.gpr', '-dsrc1', '-dsrc2', '*t.ada'])
    run([GPRNAME, '-Pprj.gpr', '-dsrc1', '-dsrc2', '*2.ada'])
    run([GPRNAME, '-Pprj.gpr', '-dsrc1', '-dsrc2', '--ignore-duplicate-files', '*.ada'])
    subprocess.run("gprbuild -p -q -P prj.gpr", shell=True)
except Exception as E:
    # Unexpected exception.  Just print the information we have.
    print('*** Error: %s' % str(E))
