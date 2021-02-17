from testsuite_support.builder_and_runner import BuilderAndRunner, GPRLS


subprocess = BuilderAndRunner()

subprocess.call([GPRLS, "-Pp"])
subprocess.call([GPRLS, "-Pgprbuild_O629_023"])
