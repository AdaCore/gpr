import glob
import json
import os.path
import shutil
from e3.env import Env
from e3.os.process import Run
from testsuite_support.builder_and_runner import BuilderAndRunner

bnr = BuilderAndRunner()

# ??? This test assumes we don't rebuild on switches changes as it's not
# implemented yet. Calls to gprclean can be removed when this is fixed.

def run(cmd):
    print("$ " + " ".join(cmd));
    if cmd[0].startswith('gpr'):
        bnr.call(cmd)
    else:
        print(bnr.simple_run([cmd], catch_error=True).out)

def check(root, pattern, switch, filter_out=None):
    with open (os.path.join(root, "jobs.json")) as fp:
        cnt = json.load (fp)
    cmds = dict((job['uid'], job['command']) for job in cnt)

    for uid in sorted(cmds.keys()):
        if pattern in uid:
            opts = [arg for arg in cmds[uid].split(" ") if arg.startswith(switch) and ((not filter_out) or (filter_out not in arg))]
            print(f"{uid}: {' '.join(opts)}")

print("Step 1 - Check -cargs")
run(["gpr2build", "-P", os.path.join("tree", "main.gpr"), "-p", "-q", "--json-summary", "-j1", "-cargs", "-O2", "-cargs:Ada", "-O1", "-cargs:C", "-O0"])
check("tree", "Compile", "-O")
print("")

print("Step 2 - Check -bargs")
run(["gpr2build", "-P", os.path.join("tree_2", "main.gpr"), "-p", "-q", "--json-summary", "-j1", "-bargs", "-O=3.bind", "-bargs:Ada", "-O=5.bind", "-bargs", "-O=4.bind", "-bargs:Ada", "-O=6.bind"])
check("tree_2", "Ada Bind", "-O")
print("")

print("Step 3 - Check -largs")
run(["gpr2build", "-P", os.path.join("tree_3", "main.gpr"), "-p", "-q", "--json-summary", "-j1", "-largs", "-Wl,--gc-sections"])
check("tree_3", "Link", "-Wl,--", "--stack=")
