import subprocess

from testsuite_support.builder_and_runner import BuilderAndRunner, GPRNAME

bnr = BuilderAndRunner()


def run(args):
    print(bnr.check_output(args).out)


# non ada sources support
try:
    run([GPRNAME, '-Pprj.gpr', '*.ada', '-f', '*.c', '-f:c', '*.clang'])
    subprocess.run("gprbuild -p -q -P prj.gpr main.ada", shell=True)
    subprocess.run("./main")
except Exception as E:
    # Unexpected exception.  Just print the information we have.
    print('*** Error: %s' % str(E))
