from testsuite_support.builder_and_runner import BuilderAndRunner, GPRLS

BuilderAndRunner().check_call([GPRLS, "-P", "p-c.gpr"])
