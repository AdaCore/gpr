from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPR2BUILD
from e3.env import Env

bnr = BuilderAndRunner()

bnr.call([GPR2BUILD, "-P", "test.gpr", "-q", "-p" ])
output = bnr.call(["sh", "-c", 'echo "break $(pwd)/main.adb:3\nrun\n" | gdb obj/main'], quiet=True).out

if "raise Constraint_Error" in output:
    print("Success")
else:
    print("Failure. Output:")
    print(output)
