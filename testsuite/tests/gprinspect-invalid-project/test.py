from testsuite_support.builder_and_runner import BuilderAndRunner, GPRINSPECT

bnr = BuilderAndRunner()

p = bnr.run([GPRINSPECT, "-Pprj1.gpr"])
print (f"{p.out}")
