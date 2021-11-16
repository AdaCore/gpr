from testsuite_support.builder_and_runner import BuilderAndRunner, GPRLS, GPRCLEAN


bnr = BuilderAndRunner()

bnr.build("add_body.gpr")
bnr.build("add_spec.gpr")

bnr.call([GPRLS, "-P", "add_body.gpr"])
print("------------")
bnr.call([GPRLS, "-P", "add_spec.gpr"])

bnr.call([GPRCLEAN, "-r", "-P", "add_body.gpr"])
bnr.call([GPRCLEAN, "-r", "-P", "add_spec.gpr"])
