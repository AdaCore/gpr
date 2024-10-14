from testsuite_support.builder_and_runner import BuilderAndRunner, GPRLS, GPRBUILD

bnr = BuilderAndRunner()

bnr.call([GPRBUILD, "-q", "-p", "-Ptest"])
bnr.call([GPRLS, "-Ptest"])
