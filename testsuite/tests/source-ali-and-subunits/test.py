import subprocess
from testsuite_support.builder_and_runner import GPRLS


subprocess.call(["gprbuild", "-f", "-q", "-k", "-p", "prj1.gpr", "-cargs",
                 "-O", "-gnatn"])
subprocess.call([GPRLS, "-Pprj1"])
