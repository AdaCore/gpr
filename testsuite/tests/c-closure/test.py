from testsuite_support.builder_and_runner import BuilderAndRunner, GPRLS, GPRBUILD

bnr = BuilderAndRunner()
bnr.check_call([GPRBUILD, "-Pprj", "-q"])
bnr.check_call([GPRLS, "-Pprj", "--closure"])
