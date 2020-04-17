import os

from e3.os.process import Run

# Set environment with project paths terminated by path separator ':' or ';'
for var in ["GPR_PROJECT_PATH", "ADA_PROJECT_PATH"]:
    os.environ[var] = os.environ.get(var, "") + os.pathsep

# Check that gpr2-projects package is able to handle such environment without
# raising an exception.
p = Run(['gpr2clean', '-Pp'])
if p.status != 0:
    print('gpr2clean returned ' + str(p.status))
    print(p.err)
    print(p.out)
else:
    print('OK')
