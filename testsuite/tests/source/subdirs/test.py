from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRLS, GPRBUILD


def execute(cmd):
    print("------------------")
    print(" ".join(cmd))
    print("")
    bnr.call(cmd)


bnr = BuilderAndRunner()

for prj in "prj1", "prj2":
    bnr.call([GPRBUILD, f"-P{prj}", "--src-subdirs=src2", "-q"])
    execute([GPRLS, f"-P{prj}", "-s", "--src-subdirs=src2"])
