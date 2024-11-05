from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRLS, GPRCLEAN, GPRBUILD

bnr = BuilderAndRunner()

bnr.run([GPRBUILD, "-p", "-q", "-Pgauges"])
bnr.call([GPRLS, "-P", "gauges", "--closure", "gauge"])
bnr.call([GPRCLEAN, "-p", "-r", "-P", "gauges"])
