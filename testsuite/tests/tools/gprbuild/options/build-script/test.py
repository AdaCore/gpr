import os.path
import glob

from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD, GPRCLEAN

bnr = BuilderAndRunner()

def run(cmd):
    print("$ " + " ".join(cmd));
    if cmd[0] in (GPRBUILD, GPRCLEAN):
        bnr.call(cmd)
    else:
        print(bnr.simple_run([cmd], catch_error=True).out)

# creation of the script
run([GPRBUILD, os.path.join("tree", "main.gpr"), "-q", "-f", "--build-script=script.sh"])
run([os.path.join("tree", "main")])
run([GPRCLEAN, os.path.join("tree", "main.gpr"), "-r"])

# now execution of the script
for file in glob.glob("tree/obj/.ada_bind_main-mapping_*.tmp"):
    print("$ cat tree/obj/.ada_bind_main-mapping_*.tmp")
    print(bnr.simple_run(["cat", file], catch_error=True).out)

try:
    run([os.environ["SHELL"], "./script.sh"])
except:
    pass
run([os.path.join("tree", "main")])
