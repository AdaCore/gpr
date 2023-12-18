import os
from testsuite_support.builder_and_runner import BuilderAndRunner, GPRLS


p = BuilderAndRunner().run(
    [GPRLS, "-U", "-P", "prj.gpr"],
    env={"GPR_PROJECT_PATH": os.getcwd() + "/files"},
)
print(p.out, end="")

