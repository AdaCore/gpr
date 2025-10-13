from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD, GPRCLEAN

bnr = BuilderAndRunner()

def execute(cmd):
    print("-------------------------")
    print(" ".join(cmd))
    print("-------------------------")
    bnr.check_call(cmd)
    print("")

# Build -> warning

execute([GPRBUILD, "prj.gpr"])

# Re-build -> warning

execute([GPRBUILD, "prj.gpr"])

# Re-build with --no-warnings-replay -> no warning

execute([GPRBUILD, "prj.gpr", "--no-warnings-replay"])

# Reset

execute([GPRCLEAN, "prj.gpr"])

# Build with --no-warnings-replay -> warning

execute([GPRBUILD, "prj.gpr", "--no-warnings-replay"])

# Re-build with --no-warnings-replay -> no warning

execute([GPRBUILD, "prj.gpr", "--no-warnings-replay"])

# Re-build without --no-warnings-replay-> warning

execute([GPRBUILD, "prj.gpr"])
