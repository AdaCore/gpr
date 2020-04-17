import subprocess

subprocess.call(["gpr2clean", "-r", "prj.gpr"])
subprocess.call(["gpr2clean", "-r", "--unchecked-shared-lib-imports",
                 "prj.gpr"])

# TODO: reactivate code below when we know the name of switch disabling the
# warnings.
if False:
    subprocess.call(["gpr2clean", "-ws", "-r",
                     "--unchecked-shared-lib-imports", "prj.gpr"])
