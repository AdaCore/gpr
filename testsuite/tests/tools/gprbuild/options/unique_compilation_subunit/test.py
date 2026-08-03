from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD

bnr = BuilderAndRunner()

cmd = [GPRBUILD, "-P", "prj.gpr", "-u", "parent-child.adb"]

print("$ " + " ".join(cmd))
out = bnr.simple_run([cmd], catch_error=False).out

if "cannot generate code" in out and "parent-child.adb" in out:
    print("PASS: -u compiled the separate parent-child.adb")
else:
    print("FAIL: the separate parent-child.adb was not the source handed to the compiler")
    print(out)
