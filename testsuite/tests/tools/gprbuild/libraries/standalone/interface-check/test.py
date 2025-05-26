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


run(f"{GPR2BUILD} prj.gpr -q")
run(f"{GPR2BUILD} prj.gpr -q")
run(f"{GPR2BUILD} -f prj2.gpr -q")
run(f"{GPR2BUILD} -f prj3.gpr -q")
run(f"{GPR2BUILD} -f prj4.gpr -q")
run(f"{GPR2BUILD} -f prj5.gpr -q")
run(f"{GPR2BUILD} -f prj6.gpr -q")
run(f"{GPR2BUILD} -f --no-indirect-imports direct/prj.gpr -k -q")
