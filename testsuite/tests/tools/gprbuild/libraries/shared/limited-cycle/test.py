from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD

bnr = BuilderAndRunner()
result = bnr.run([GPRBUILD, "-P", "p.gpr", "-p", "-q"])

assert result.status != 0, result.out
assert "[Link]" in result.out and " failed." in result.out, result.out
print("Link failed as expected")
