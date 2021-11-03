from testsuite_support.builder_and_runner import BuilderAndRunner, GPRLS

BuilderAndRunner().check_output([GPRLS, "-Pbug"])
