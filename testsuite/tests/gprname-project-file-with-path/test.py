from testsuite_support.builder_and_runner import BuilderAndRunner, GPRNAME
import subprocess

bnr = BuilderAndRunner()


def run(args):
    print(bnr.check_output(args).out)


# allow -P with path
try:
    run([GPRNAME, '-PA/p', "-d", "src", '*.ada'])
    subprocess.run("gprbuild -p -q -P A/p.gpr", shell=True)
    subprocess.run("./A/main")
except Exception as E:
    print('*** Error: %s' % str(E))
