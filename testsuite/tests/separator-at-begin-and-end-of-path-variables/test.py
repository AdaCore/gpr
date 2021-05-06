import os

from testsuite_support.builder_and_runner import BuilderAndRunner, GPRCLEAN

# Set environment with paths starting or terminated by path separator ':' or ';'
for var in ["PATH", "GPR_PROJECT_PATH", "ADA_PROJECT_PATH"]:
    os.environ[var] = (
        os.pathsep + " " + os.pathsep + os.environ.get(var, "") + os.pathsep
    )

# Check that gpr2-projects package is able to handle such environment without
# raising an exception.
p = BuilderAndRunner().run([GPRCLEAN, "-Pp"])
if p.status != 0:
    print("gprclean returned " + str(p.status))
    print(p.err)
    print(p.out)
else:
    print("OK")
