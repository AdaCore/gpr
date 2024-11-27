import os.path

from testsuite_support.builder_and_runner import BuilderAndRunner

bnr = BuilderAndRunner()

print("Automatic for simple projects:")
bnr.call(["gpr2build", "-P", os.path.join("tree", "simple.gpr")])

print("Without -p:")
bnr.call(["gpr2build", "-P", os.path.join("tree", "main.gpr")])

print("With -p:")
bnr.call(["gpr2build", "-P", os.path.join("tree", "main.gpr"), "-p"])
