import os
from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPR2BUILD

bnr = BuilderAndRunner()

# ensure .ali and .o files are there for the trees used to test
bnr.call([GPR2BUILD, "-Paggr.gpr", "-p", "-q"])
p = bnr.simple_run([os.path.join ("tree1", "a")], catch_error=True)
print(p.out, end="")
p =bnr.simple_run([os.path.join ("tree2", "a")], catch_error=True)
print(p.out, end="")
