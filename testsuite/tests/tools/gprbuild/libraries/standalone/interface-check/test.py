from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPR2BUILD

bnr = BuilderAndRunner()


def run(cmd):
    if isinstance (cmd, str):
        args = cmd.split(" ")
    else:
        args = cmd
    print("$ " + " ".join(args))
    if args[0] == GPR2BUILD:
        bnr.call(args)
    else:
        print(bnr.simple_run(args, catch_error=True).out)

stdarg = "-q -j1"

run(f"{GPR2BUILD} prj.gpr {stdarg}")
run(f"{GPR2BUILD} prj.gpr {stdarg}")
run(f"{GPR2BUILD} -f prj2.gpr {stdarg}")
run(f"{GPR2BUILD} -f prj3.gpr {stdarg}")
run(f"{GPR2BUILD} -f prj4.gpr {stdarg}")
run(f"{GPR2BUILD} -f prj5.gpr {stdarg}")
run(f"{GPR2BUILD} -f prj6.gpr {stdarg}")
run(f"{GPR2BUILD} -f --no-indirect-imports direct/prj.gpr -k {stdarg}")
