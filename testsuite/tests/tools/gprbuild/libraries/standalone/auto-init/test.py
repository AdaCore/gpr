import os
from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD, GPRCLEAN


bnr = BuilderAndRunner()

def run(cmd):
    print("$ " + " ".join(cmd));
    if cmd[0] == GPRBUILD:
        bnr.call(cmd)
    else:
        print(bnr.simple_run([cmd], catch_error=True).out)

# Let windows find the dynamic lib
os.environ["PATH"] = os.pathsep.join(
    [os.path.join(os.getcwd(), "sal", "lib"), os.environ["PATH"]]
)

run ([GPRBUILD, "-q", "-Pimplicit_elaboration.gpr", "-p"])
run(["./main"])
run ([GPRCLEAN, "-q", "-Pimplicit_elaboration.gpr", "-r"])

run ([GPRBUILD, "-q", "-Pexplicit_elaboration.gpr", "-p"])
run(["./main"])
run ([GPRCLEAN, "-q", "-Pexplicit_elaboration.gpr", "-r"])

run ([GPRBUILD, "-q", "-Perror.gpr", "-p"])
run(["./main"])
run ([GPRCLEAN, "-q", "-Perror.gpr", "-r"])
