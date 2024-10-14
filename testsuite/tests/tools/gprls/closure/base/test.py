from testsuite_support.builder_and_runner import BuilderAndRunner, GPRLS, GPRBUILD

bnr = BuilderAndRunner()

bnr.run([GPRBUILD, "-p", "-q", "-Pprj.gpr"])
bnr.call([GPRLS, "-P", "prj.gpr", "--closure"])
bnr.call([GPRLS, "-P", "prj.gpr", "main2.adb", "--closure"])
bnr.call([GPRLS, "-P", "prj.gpr", "toto.adb", "--closure"])
bnr.run([GPRBUILD, "-p", "-q", "-Pprj.gpr", "main2.adb"])
bnr.call([GPRLS, "-P", "prj.gpr", "main.ali", "main2.adb", "--closure"])
bnr.call([GPRLS, "-P", "prj.gpr", "main.adb", "toto.adb", "--closure"])
