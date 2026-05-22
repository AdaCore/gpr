from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD, GPRCLEAN

bnr = BuilderAndRunner()

def run(cmd):
    print("$ " + " ".join(cmd))
    if cmd[0] in (GPRBUILD, GPRCLEAN):
        bnr.call(cmd)
    else:
        print(bnr.simple_run([cmd], catch_error=True).out)

run([GPRBUILD, "-f", "-q", "-P", "comperr.gpr", "-o", "drove", "driver", "-j1"])
run([GPRBUILD, "-f", "-q", "-P", "badprj.gpr", "-o", "drove", "driver", "-j1"])
run([GPRBUILD, "-f", "-q", "-P", "prj.gpr", "-o", "drove", "driver", "-j1"])
run([GPRBUILD, "-f", "-q", "-P", "lprj.gpr", "-o", "drove", "driver", "-j1"])
run([GPRBUILD, "-f", "-q", "-P", "glprj.gpr", "-o", "drove", "driver", "-j1"])
