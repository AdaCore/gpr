import subprocess

subprocess.call(["gpr2clean", "-r", "prj.gpr"])
subprocess.call(["gpr2clean", "-r", "--unchecked-shared-lib-imports", "prj.gpr"])
