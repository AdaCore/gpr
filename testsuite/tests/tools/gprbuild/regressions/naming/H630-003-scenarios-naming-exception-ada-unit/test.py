from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD, GPRCLEAN

bnr = BuilderAndRunner()

def run(cmd):
    print("$ " + " ".join(cmd))
    if cmd[0] in (GPRBUILD, GPRCLEAN):
        bnr.call(cmd)
    else:
        print(bnr.simple_run([cmd], catch_error=True).out)

run([GPRBUILD, "-c", "-q", "-P", "simulation.gpr", "-XProduct=A"])
run([GPRBUILD, "-c", "-P", "simulation.gpr", "-XProduct=C"])

run([GPRCLEAN, "-q", "-P", "simulation.gpr", "-XProduct=A"])
run([GPRCLEAN, "-q", "-P", "simulation.gpr", "-XProduct=C"])

run([GPRBUILD, "-c", "-q", "-P", "simulation.gpr", "-XProduct=A"])
run([GPRBUILD, "-c", "-P", "simulation.gpr", "-XProduct=B"])

run([GPRCLEAN, "-q", "-P", "simulation.gpr", "-XProduct=A"])
run([GPRCLEAN, "-q", "-P", "simulation.gpr", "-XProduct=B"])

run([GPRBUILD, "-c", "-q", "-P", "simulation.gpr", "-XProduct=A"])
run([GPRBUILD, "-c", "-P", "simulation.gpr", "-XProduct=D"])

run([GPRCLEAN, "-q", "-P", "simulation.gpr", "-XProduct=A"])
run([GPRCLEAN, "-q", "-P", "simulation.gpr", "-XProduct=D"])
