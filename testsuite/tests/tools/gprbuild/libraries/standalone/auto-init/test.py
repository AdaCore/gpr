import os
from testsuite_support.builder_and_runner import BuilderAndRunner

bnr = BuilderAndRunner()

def run(cmd):
    print("$ " + " ".join(cmd));
    if cmd[0] == "gpr2build":
        bnr.call(cmd)
    else:
        print(bnr.simple_run([cmd], catch_error=True).out)

# Let windows find the dynamic lib
os.environ["PATH"] = os.pathsep.join(
    [os.path.join(os.getcwd(), "sal", "lib"), os.environ["PATH"]]
)

run (["gpr2build", "-q", "-Pimplicit_elaboration.gpr", "-p"])
run(["./main"])
run (["gpr2clean", "-q", "-Pimplicit_elaboration.gpr", "-r"])

run (["gpr2build", "-q", "-Pexplicit_elaboration.gpr", "-p"])
run(["./main"])
run (["gpr2clean", "-q", "-Pexplicit_elaboration.gpr", "-r"])

run (["gpr2build", "-q", "-Perror.gpr", "-p"])
run(["./main"])
run (["gpr2clean", "-q", "-Perror.gpr", "-r"])

