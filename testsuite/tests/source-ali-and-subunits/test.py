import subprocess
subprocess.call(["gprbuild", "-f", "-q", "-k", "-p", "prj1.gpr", "-cargs",
                 "-O", "-gnatn"])
subprocess.call(["gprls", "-Pprj1"])
