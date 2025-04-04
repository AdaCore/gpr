import os.path

from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPR2BUILD, GPR2CLEAN

bnr = BuilderAndRunner()

def run(cmd):
    print("$ " + " ".join(cmd));
    if cmd[0] == "gpr2build":
        bnr.call(cmd)
    else:
        print(bnr.simple_run([cmd], catch_error=True).out)

# creation of the script
run([GPR2BUILD, os.path.join("tree", "main.gpr"), "-q", "-f", "--build-script=script.sh"])
run([os.path.join("tree", "main")])
run([GPR2CLEAN, os.path.join("tree", "main.gpr"), "-r"])

# now execution of the script
run(["cat", "tree/obj/.ada_bind_main-mapping.tmp"])
try:
    run([os.environ["SHELL"], "./script.sh"])
except:
    pass
run([os.path.join("tree", "main")])
