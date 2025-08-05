from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD

bnr = BuilderAndRunner()

bnr.setup_tmpdir("projects")

print("Default compiler:")
bnr.call ([GPRBUILD, "-Pprojects/prj", "-c", "-v"])
print("")

print ("Substituted compiler:")
bnr.call ([GPRBUILD, "-Pprojects/prj", "--compiler-subst=Ada,fake-compiler-gcc", "-c", "-v"])