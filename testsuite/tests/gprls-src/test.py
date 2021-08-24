from testsuite_support.builder_and_runner import BuilderAndRunner, GPRLS, GPRCLEAN


bnr = BuilderAndRunner()

# Without atrifact directories
bnr.call([GPRLS, "-d", "-P", "pa.gpr", "p0"])
bnr.call([GPRLS, "-s", "-d", "-P", "pa.gpr", "p0"])

bnr.call(["gprbuild", "-j0", "-q", "-P", "pa.gpr"])
bnr.call([GPRLS, "-d", "-P", "pa.gpr", "p0"])
bnr.call([GPRLS, "-d", "-a0", "-P", "pa.gpr", "p0"])
bnr.call([GPRCLEAN, "-r", "-P", "pa.gpr"])

# With empty atrifact directories
bnr.call([GPRLS, "-d", "-P", "pa.gpr", "p5_1"])
bnr.call([GPRLS, "-s", "-d", "-P", "pa.gpr", "p5_1"])
bnr.call([GPRLS, "-s", "--closure", "-P", "pa.gpr", "p6_2"])
bnr.call([GPRLS, "-s", "-d", "-P", "pa.gpr", "p7_3"])
bnr.call([GPRLS, "--source-parser", "-U", "-P", "pa.gpr"])
