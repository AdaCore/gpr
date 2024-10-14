import os
import subprocess

from testsuite_support.builder_and_runner import BuilderAndRunner, GPRINSTALL, GPRLS, GPRBUILD

bnr = BuilderAndRunner()


inst_prefix = "--prefix=" + os.getcwd() + "/inst"

# Build & Install library with object from ASM source file
bnr.check_output([GPRBUILD, "-p",  "-q", "installed_asm_object.gpr"])
bnr.run([GPRINSTALL, "-p", inst_prefix, "installed_asm_object.gpr"])

# Check gprls is working fine of this installed library
os.chdir("./inst")
p = bnr.run(
    [GPRLS, "-P", "installed_asm_object.gpr"],
    env={"GPR_PROJECT_PATH": os.getcwd() + "/share/gpr"},
)
print(p.out, end="")
