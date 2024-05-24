from testsuite_support.builder_and_runner import BuilderAndRunner, GPRLS
import os

bnr = BuilderAndRunner()

bnr.run(["gprbuild", "-p", "-q", "-Pprj.gpr"])
bnr.call([GPRLS, "-P", "prj.gpr", "--closure"])
bnr.call([GPRLS, "-P", "prj.gpr", "main2.adb", "--closure"])
bnr.call([GPRLS, "-P", "prj.gpr", "toto.adb", "--closure"])
bnr.run(["gprbuild", "-p", "-q", "-Pprj.gpr", "main2.adb"])
bnr.call([GPRLS, "-P", "prj.gpr", "main.ali", "main2.adb", "--closure"])
bnr.call([GPRLS, "-P", "prj.gpr", "main.adb", "toto.adb", "--closure"])

####################
# Predefined units #
####################

p = bnr.check_output([GPRLS, "-a0", "-Pprj", "main.adb", "-v", "--closure"])
if "system.ads" in p.out:
    print("runtime correctly printed [1/4]")

p = bnr.check_output([GPRLS, "-a", "-Pprj", "main.adb", "-v", "--closure"])
if os.path.join("adainclude", "system.ads") in p.out:
    print("runtime correctly printed [2/4]")

p = bnr.check_output([GPRLS, "-a0", "-Pprj", "main.adb", "--closure"])
if "system.ads" in p.out in p.out:
    print("runtime correctly printed [3/4]")

p = bnr.check_output([GPRLS, "-a", "-Pprj", "main.adb", "--closure"])
if os.path.join("adainclude", "system.ads") in p.out:
    print("runtime correctly printed [4/4]")
