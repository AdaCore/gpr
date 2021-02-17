import subprocess

from testsuite_support.builder_and_runner import GPRLS


subprocess.call(["gprbuild", "-q", "-p", "-Ptest"])
subprocess.call([GPRLS, "-Ptest"])
