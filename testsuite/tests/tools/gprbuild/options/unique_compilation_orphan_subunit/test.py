from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD

bnr = BuilderAndRunner()
for source in ("parent-child.adb", "parent-child-nested.adb"):
    cmd = [GPRBUILD, "-Pprj.gpr", "-c", "-u", source]
    print("$ " + " ".join(cmd))
    result = bnr.simple_run(cmd, catch_error=False)
    print(result.out, end="")
    print("exit status:", result.status)
