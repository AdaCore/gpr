from testsuite_support.builder_and_runner import BuilderAndRunner, GPRLS

bnr = BuilderAndRunner()
bnr.check_call(["gprbuild", "-Pprj", "-q"])
bnr.check_call([GPRLS, "-Pprj", "--closure"])
