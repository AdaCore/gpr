from testsuite_support.builder_and_runner import BuilderAndRunner, GPRLS, GPRCLEAN

bnr = BuilderAndRunner()

bnr.run(["gprbuild", "-p", "-q", "-Pgauges"])
bnr.call([GPRLS, "-P", "gauges", "--closure", "gauge"])
bnr.call([GPRCLEAN, "-p", "-r", "-P", "gauges"])
