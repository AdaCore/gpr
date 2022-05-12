from testsuite_support.builder_and_runner import BuilderAndRunner, GPRLS


bnr = BuilderAndRunner()

bnr.call([GPRLS, "-Pp"])
bnr.call([GPRLS, "-Pgprbuild_O629_023"])
