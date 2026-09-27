import os
from testsuite_support.builder_and_runner import BuilderAndRunner

bnr = BuilderAndRunner()

bnr.build(project="test.gpr", args=["-p", "-q"])

# Create the tree's object directory where signatures are stored.
os.makedirs(os.path.join("tree", "obj"), exist_ok=True)

proc = bnr.call(["./test"], quiet=True)
print(proc.out.strip())
