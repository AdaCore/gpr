from testsuite_support.builder_and_runner import GPRLS
import subprocess


subprocess.check_call([GPRLS, "-Pprj.gpr"])
