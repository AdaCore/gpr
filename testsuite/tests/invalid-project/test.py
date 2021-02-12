from testsuite_support.builder_and_runner import BuilderAndRunner


subprocess = BuilderAndRunner()

subprocess.call(["gprls", "-Pp"])
subprocess.call(["gprls", "-Pgprbuild_O629_023"])
