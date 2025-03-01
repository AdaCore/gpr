from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRLS


def execute(cmd):
    print("------------------")
    print(" ".join(cmd))
    print("")
    bnr.call(cmd)


bnr = BuilderAndRunner()
execute([GPRLS, "-Ptest1/p"])
execute([GPRLS, "-Ptest2/p"])
