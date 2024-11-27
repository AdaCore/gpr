import os
from testsuite_support.builder_and_runner import BuilderAndRunner

bnr = BuilderAndRunner()

def run(cmd):
    print("$ " + " ".join(cmd));
    if cmd[0] == "gpr2build":
        bnr.call(cmd)
    else:
        print(bnr.simple_run([cmd], catch_error=True).out)

run(["gpr2build", "-q", "-P", os.path.join("tree", "demo.gpr"), "-p"])
run(["./tree/main"])
