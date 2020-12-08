import subprocess

subprocess.call(["gprbuild", "-q", "-p", "-Ptest"])
subprocess.call(["gpr2ls", "-Ptest"])
