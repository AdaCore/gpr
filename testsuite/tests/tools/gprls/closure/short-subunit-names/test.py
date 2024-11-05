from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRLS, GPRBUILD

bnr = BuilderAndRunner()

try:

    bnr.run([GPRBUILD, "-p", "-q", "-Pp.gpr"])
    bnr.call([GPRLS, "-P", "p.gpr", "--closure"])

except Exception as E:
    # Unexpected exception.  Just print the information we have.
    print('*** Error: %s' % str(E))
