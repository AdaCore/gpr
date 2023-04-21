import subprocess

from testsuite_support.builder_and_runner import BuilderAndRunner, GPRNAME

bnr = BuilderAndRunner()


def run(args):
    out = bnr.check_output(args).out.split("\n")
    if "warning: duplicate unit" in out[0] and "warning: generated Naming package needs to be reviewed manually" in out[1]:
        print("TEST OK !")
    else:
        print("TEST KO ! warnings were not outputed")


# ignore-duplicate-files switch support
try:
    run([GPRNAME, '-Pprj.gpr', '-dsrc', '*.ad?'])
except Exception as E:
    # Unexpected exception.  Just print the information we have.
    print('*** Error: %s' % str(E))
