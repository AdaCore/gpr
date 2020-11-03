from testsuite_support.builder_and_runner import BuilderAndRunner


subprocess = BuilderAndRunner()

subprocess.call(["gpr2ls", "-Pp"])
subprocess.call(["gpr2ls", "-Pgprbuild_O629_023"])
