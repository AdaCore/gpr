from testsuite_support.builder_and_runner import BuilderAndRunner, GPRINSTALL


br = BuilderAndRunner()
br.build("src/lext_0.gpr")
br.build("src/lagg.gpr")
br.call([GPRINSTALL, "-P", "src/lagg.gpr", "-p", "--prefix=gpr"])
br.build("test.gpr", ["-aP", "gpr/share/gpr", "-largs", "-L", "src/lext"])
br.call(["./test"])
