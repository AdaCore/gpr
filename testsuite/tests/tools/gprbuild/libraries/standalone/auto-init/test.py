import os
from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPR2BUILD, GPR2CLEAN


bnr = BuilderAndRunner()

def run(cmd):
    print("$ " + " ".join(cmd));
    if cmd[0] == GPR2BUILD:
        bnr.call(cmd)
    else:
        print(bnr.simple_run([cmd], catch_error=True).out)

# Let windows find the dynamic lib
os.environ["PATH"] = os.pathsep.join(
    [os.path.join(os.getcwd(), "sal", "lib"), os.environ["PATH"]]
)

run ([GPR2BUILD, "-q", "-Pimplicit_elaboration.gpr", "-p"])
run(["./main"])
run ([GPR2CLEAN, "-q", "-Pimplicit_elaboration.gpr", "-r"])

run ([GPR2BUILD, "-q", "-Pexplicit_elaboration.gpr", "-p"])
run(["./main"])
run ([GPR2CLEAN, "-q", "-Pexplicit_elaboration.gpr", "-r"])

run ([GPR2BUILD, "-q", "-Perror.gpr", "-p"])
run(["./main"])
run ([GPR2CLEAN, "-q", "-Perror.gpr", "-r"])
