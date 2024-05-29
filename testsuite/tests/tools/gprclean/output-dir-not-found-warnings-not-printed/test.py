from testsuite_support.builder_and_runner import BuilderAndRunner, GPRCLEAN

bnr = BuilderAndRunner()

print("gprclean -P adaexe")
bnr.call([GPRCLEAN, "-P", "adaexe"])
print("gprclean -P adalib")
bnr.call([GPRCLEAN, "-q", "-P", "adalib"])
