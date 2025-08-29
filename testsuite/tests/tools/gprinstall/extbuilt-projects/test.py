import os
from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRINSTALL, GPRBUILD

bnr = BuilderAndRunner()

def run(cmd):
    print(f"$ {cmd}")
    cmd = cmd.split(' ')
    if cmd[0] in (GPRBUILD, GPRINSTALL):
        out = bnr.check_output(cmd).out
    else:
        out = bnr.simple_run([cmd], catch_error=True).out
    print(out, end="")

run(f"{GPRBUILD} lib/lib1.gpr -q")
run(f"{GPRBUILD} lib/lib2.gpr -q")

for lib in ("lib1e.gpr", "lib2e.gpr"):
    run(f"{GPRINSTALL} -p --prefix={os.getcwd()}/inst lib/{lib}")

if os.path.exists("inst"):
    run(f"{GPRBUILD} -aPinst/share/gpr -q main.gpr")
    run("./main")
else:
    print("error: the installation directory is missing after call to gprinstall")
