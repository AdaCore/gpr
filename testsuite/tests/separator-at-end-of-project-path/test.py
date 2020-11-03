import os

from testsuite_support.builder_and_runner import BuilderAndRunner

# Set environment with project paths terminated by path separator ':' or ';'
for var in ["GPR_PROJECT_PATH", "ADA_PROJECT_PATH"]:
    os.environ[var] = os.environ.get(var, "") + os.pathsep

# Check that gpr2-projects package is able to handle such environment without
# raising an exception.
p = BuilderAndRunner().run(['gpr2clean', '-Pp'])
if p.status != 0:
    print('gpr2clean returned ' + str(p.status))
    print(p.err)
    print(p.out)
else:
    print('OK')
