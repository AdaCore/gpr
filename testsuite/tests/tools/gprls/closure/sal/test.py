from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRLS, GPR2CLEAN, GPR2BUILD

bnr = BuilderAndRunner()

bnr.run([GPR2BUILD, "-p", "-q", "-Pgauges"])
bnr.call([GPRLS, "-P", "gauges", "--closure", "gauge"])
bnr.call([GPR2CLEAN, "-p", "-r", "-P", "gauges"])
