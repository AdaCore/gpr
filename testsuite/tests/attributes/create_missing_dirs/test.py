import os
import shutil

from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD

bnr = BuilderAndRunner()

obj = "obj"
if os.path.isdir(obj):
    shutil.rmtree(obj)

bnr.run([GPRBUILD, "-q", "-Pprj.gpr"])

if os.path.isdir(obj):
    print("Object directory created")
else:
    print("ERROR: object directory was not created")
