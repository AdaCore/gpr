import os
import stat

from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD

bnr = BuilderAndRunner()
saved_umask = os.umask(0o022)

try:
    bnr.check_call([GPRBUILD, "-p", "-q", "-Plib.gpr"])
finally:
    os.umask(saved_umask)

assert os.path.isfile("ali/pkg.ali")
print(f"ALI directory mode: {stat.S_IMODE(os.stat('ali').st_mode):04o}")
