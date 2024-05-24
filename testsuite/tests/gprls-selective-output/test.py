import os
from e3.os.process import Run
from testsuite_support.builder_and_runner import BuilderAndRunner, GPRLS

bnr = BuilderAndRunner()

Run(["gprbuild", "-p", "-q", "-Pprj"])

p = bnr.check_output([GPRLS, "-o", "-u", "-Pprj"])
print(p.out)
p = bnr.check_output([GPRLS, "-s", "-u", "-Pprj"])
print(p.out)
p = bnr.check_output([GPRLS, "-s", "-o", "-Pprj"])
print(p.out)

####################
# Predefined units #
####################

p = bnr.check_output([GPRLS, "-s", "-a0", "-Pprj", "-v"])
if "system.ads" in p.out and "depends upon runtime" in p.out:
    print("runtime correctly printed [1/4]")

p = bnr.check_output([GPRLS, "-s", "-a", "-Pprj", "-v"])
if (
    os.path.join("adainclude", "system.ads") in p.out
    and "depends upon runtime" in p.out
):
    print("runtime correctly printed [2/4]")

p = bnr.check_output([GPRLS, "-s", "-a0", "-Pprj"])
if "system.ads" in p.out in p.out:
    print("runtime correctly printed [3/4]")

p = bnr.check_output([GPRLS, "-s", "-a", "-Pprj"])
if os.path.join("adainclude", "system.ads") in p.out:
    print("runtime correctly printed [4/4]")
