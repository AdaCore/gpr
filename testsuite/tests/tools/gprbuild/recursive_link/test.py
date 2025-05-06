from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPR2BUILD

bnr = BuilderAndRunner()

def run(cmd):
    print("$ " + " ".join(cmd))
    bnr.call(cmd)

run([GPR2BUILD, "-q", "prj.gpr"])
run(["./main"])
