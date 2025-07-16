from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD

bnr = BuilderAndRunner()


def run(cmd):
    if isinstance (cmd, str):
        args = cmd.split(" ")
    else:
        args = cmd
    print("$ " + " ".join(args))
    if args[0] == GPRBUILD:
        bnr.call(args)
    else:
        print(bnr.simple_run(args, catch_error=True).out)

stdarg = "-q -j1"

run(f"{GPRBUILD} prj.gpr {stdarg}")
run(f"{GPRBUILD} prj.gpr {stdarg}")
run(f"{GPRBUILD} -f prj2.gpr {stdarg}")
run(f"{GPRBUILD} -f prj3.gpr {stdarg}")
run(f"{GPRBUILD} -f prj4.gpr {stdarg}")
run(f"{GPRBUILD} -f prj5.gpr {stdarg}")
run(f"{GPRBUILD} -f prj6.gpr {stdarg}")
run(f"{GPRBUILD} -f --no-indirect-imports direct/prj.gpr -k {stdarg}")
