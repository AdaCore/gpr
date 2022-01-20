import os
import subprocess

from testsuite_support.builder_and_runner import BuilderAndRunner, GPRINSTALL, GPRLS


inst_prefix = "--prefix=" + os.getcwd() + "/inst"

# Build & Install library with object from ASM source file
subprocess.check_output("gprbuild -p -q installed_asm_object.gpr", shell=True)
BuilderAndRunner().run([GPRINSTALL, "-p", inst_prefix, "installed_asm_object.gpr"])

# Check gprls is working fine of this installed library
os.chdir("./inst")
p = BuilderAndRunner().run(
    [GPRLS, "-P", "installed_asm_object.gpr"],
    env={"GPR_PROJECT_PATH": os.getcwd() + "/share/gpr"},
)
print(p.out, end="")
