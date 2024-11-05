from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD, GPRINSTALL


br = BuilderAndRunner()
br.run([GPRBUILD, "-p", "-q", "-Psrc/lext_0.gpr"])
br.run([GPRBUILD, "-p", "-q", "-Psrc/lagg.gpr"])
br.call([GPRINSTALL, "-P", "src/lagg.gpr", "-p", "--prefix=gpr"])
br.run([GPRBUILD, "-p", "-q", "-Ptest.gpr", "-aP",
        "gpr/share/gpr", "-largs", "-L", "src/lext"])
br.call(["./test"])
