from testsuite_support.builder_and_runner import BuilderAndRunner, GPRLS, GPRCLEAN


def execute(cmd):
    print("------------------")
    print(" ".join(cmd))
    print("")
    bnr.call(cmd)


bnr = BuilderAndRunner()

# Without atrifact directories
execute([GPRLS, "-d", "-P", "pa.gpr", "p0"])
execute([GPRLS, "-s", "-d", "-P", "pa.gpr", "p0"])

print("building...")
bnr.call(["gprbuild", "-q", "-P", "pa.gpr"])
execute([GPRLS, "-d", "-P", "pa.gpr", "p0"])
execute([GPRLS, "-d", "-a0", "--debugF", "-P", "pa.gpr", "p0"])
execute([GPRLS, "-s", "--closure", "-P", "pa.gpr", "p6_2"])

print("cleaning...")
bnr.call([GPRCLEAN, "-r", "-P", "pa.gpr"])

# With empty atrifact directories
execute([GPRLS, "-d", "-P", "pa.gpr", "p5_1"])
execute([GPRLS, "-s", "-d", "-P", "pa.gpr", "p5_1"])
execute([GPRLS, "-s", "--closure", "-P", "pa.gpr", "p6_2"])
execute([GPRLS, "-s", "-d", "-P", "pa.gpr", "p7_3"])
execute([GPRLS, "--source-parser", "-U", "-P", "pa.gpr"])
