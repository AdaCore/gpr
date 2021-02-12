import subprocess

subprocess.call(["gprbuild", "-q", "-p", "-Ptest"])
subprocess.call(["gprls", "-Ptest"])
