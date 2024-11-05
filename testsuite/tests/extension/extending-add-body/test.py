from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRLS, GPRCLEAN, GPRBUILD


bnr = BuilderAndRunner()

bnr.run([GPRBUILD, "-p", "-q", "-Padd_body.gpr"])
bnr.run([GPRBUILD, "-p", "-q", "-Padd_spec.gpr"])

bnr.call([GPRLS, "-P", "add_body.gpr"])
print("------------")
bnr.call([GPRLS, "-P", "add_spec.gpr"])

bnr.call([GPRCLEAN, "-r", "-P", "add_body.gpr"])
bnr.call([GPRCLEAN, "-r", "-P", "add_spec.gpr"])
