import os
import subprocess
from testsuite_support.builder_and_runner import BuilderAndRunner

# For default

subprocess.run("gprinstall -q  -p --prefix=install install.gpr",
               shell=True)
