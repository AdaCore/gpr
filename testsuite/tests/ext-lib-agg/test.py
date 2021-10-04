from testsuite_support.builder_and_runner import BuilderAndRunner, GPRINSTALL


br = BuilderAndRunner()
br.run(["gprbuild", "-p", "-q", "-Psrc/lext_0.gpr"])
br.run(["gprbuild", "-p", "-q", "-Psrc/lagg.gpr"])
br.call([GPRINSTALL, "-P", "src/lagg.gpr", "-p", "--prefix=gpr"])
br.run(["gprbuild", "-p", "-q", "-Ptest.gpr", "-aP",
        "gpr/share/gpr", "-largs", "-L", "src/lext"])
br.call(["./test"])
