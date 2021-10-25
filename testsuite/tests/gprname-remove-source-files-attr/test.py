import subprocess

from testsuite_support.builder_and_runner import BuilderAndRunner, GPRNAME

bnr = BuilderAndRunner()


def run(args):
    print(bnr.check_output(args).out)


# source_files attributes should be removed
try:
    run([GPRNAME, '-Pprj.gpr', '*.ada'])
    subprocess.run("gprbuild -q -P prj.gpr", shell=True)
except Exception as E:
    print('*** Error: %s' % str(E))
