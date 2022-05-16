from testsuite_support.builder_and_runner import BuilderAndRunner, GPRLS

bnr = BuilderAndRunner()

bnr.call(["gprbuild", "-q", "-p", "-Ptest"])
bnr.call([GPRLS, "-Ptest"])
