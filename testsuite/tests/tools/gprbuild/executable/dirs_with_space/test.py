import os
from e3.fs import mv
from testsuite_support.builder_and_runner import BuilderAndRunner

bnr = BuilderAndRunner()

def run(cmd):
    print("$ " + " ".join(cmd));
    if cmd[0] == "gpr2build":
        bnr.call(cmd)
    else:
        print(bnr.simple_run([cmd], catch_error=True).out)

os.mkdir(os.path.join("tree", "dir with spaces"))
mv(os.path.join("tree", "foo.c"), os.path.join("tree", "dir with spaces"))

run(["gpr2build", "-q", "-P", os.path.join("tree", "demo.gpr"), "-p"])
run(["./tree/main"])
