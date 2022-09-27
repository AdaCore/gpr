from testsuite_support.builder_and_runner import BuilderAndRunner, GPRCLEAN

bnr = BuilderAndRunner()

bnr.call([GPRCLEAN, "-p"])
