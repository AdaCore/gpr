from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD, GPRCLEAN

bnr = BuilderAndRunner()

bnr.build("project_paths.gpr", args=["-p"])

p = bnr.call(["./main"], quiet=True)
for line in p.out.splitlines():
    # Prevent displaying search paths when a project file is not found
    if not line.startswith("   -"):
        print(line)