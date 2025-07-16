from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD

bnr = BuilderAndRunner()

def run(cmd):
    print("$ " + " ".join(cmd))
    bnr.call(cmd)

run([GPRBUILD, "-p", "-Ptree/prj"])
run([GPRBUILD, "-p", "-Ptree/prj2"])
run([GPRBUILD, "-p", "-Ptree/prj3"])
run([GPRBUILD, "-p", "-Ptree/prj4"])
