from testsuite_support.builder_and_runner import BuilderAndRunner

bnr = BuilderAndRunner()

def run(cmd):
    print("$ " + " ".join(cmd))
    bnr.call(cmd)

run(["gpr2build", "-p", "-Ptree/prj"])
run(["gpr2build", "-p", "-Ptree/prj2"])
run(["gpr2build", "-p", "-Ptree/prj3"])
run(["gpr2build", "-p", "-Ptree/prj4"])
