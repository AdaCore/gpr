import subprocess

from testsuite_support.builder_and_runner import BuilderAndRunner, GPRNAME

bnr = BuilderAndRunner()


def run(args):
    print(bnr.check_output(args).out)


# no abssolute path should be generated
try:
    run([GPRNAME, '-Pprj.gpr', '*.ada'])
    subprocess.run("gprbuild -p -q -P prj.gpr toto.ada", shell=True)
    subprocess.run("./toto")
    subprocess.run("gprbuild -p -q -P prj.gpr toto.ada", shell=True)
    subprocess.run("./toto")
except Exception as E:
    print('*** Error: %s' % str(E))
