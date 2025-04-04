from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPR2BUILD

bnr = BuilderAndRunner()

def run(cmd):
    print("$ " + " ".join(cmd))
    bnr.call(cmd)

run([GPR2BUILD, "-p", "-Ptree/prj"])
run([GPR2BUILD, "-p", "-Ptree/prj2"])
run([GPR2BUILD, "-p", "-Ptree/prj3"])
run([GPR2BUILD, "-p", "-Ptree/prj4"])
