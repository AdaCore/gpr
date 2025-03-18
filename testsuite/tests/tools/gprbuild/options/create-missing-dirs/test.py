import os.path

from testsuite_support.builder_and_runner import BuilderAndRunner

bnr = BuilderAndRunner()

print("Automatic for simple projects:")
bnr.call(["gpr2build", "-P", os.path.join("tree", "simple.gpr"), "-j1"])

print("Without -p:")
bnr.call(["gpr2build", "-P", os.path.join("tree", "main.gpr"), "-j1"])

print("With -p:")
bnr.call(["gpr2build", "-P", os.path.join("tree", "main.gpr"), "-p", "-j1"])
