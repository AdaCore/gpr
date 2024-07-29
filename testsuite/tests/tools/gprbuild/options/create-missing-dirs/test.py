import os.path

from testsuite_support.builder_and_runner import BuilderAndRunner

bnr = BuilderAndRunner()

print("Without -p:")
proc = bnr.call(["gpr2build", "-P", os.path.join("tree", "main.gpr")])

print("With -p:")
proc = bnr.call(["gpr2build", "-P", os.path.join("tree", "main.gpr"), "-p"])
