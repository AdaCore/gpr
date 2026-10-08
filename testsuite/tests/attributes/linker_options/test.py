from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD


bnr = BuilderAndRunner()
bnr.check_call([GPRBUILD, "-q", "-p", "-P", "mylib.gpr"])
print("Shared library built successfully")

p = bnr.run(
    [GPRBUILD, "-q", "-p", "-P", "app.gpr"], catch_error=False
)
assert p.status != 0, "The importing executable should fail to link"
assert "nonexistent-library.a" in p.out, p.out
print("Importing executable failed on the nonexistent archive as expected")
