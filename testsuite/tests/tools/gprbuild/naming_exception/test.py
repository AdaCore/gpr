from testsuite_support.builder_and_runner import BuilderAndRunner

bnr = BuilderAndRunner()

def run(cmd):
    print("$ " + " ".join(cmd));
    if cmd[0] == "gpr2build":
        bnr.call(cmd)
    else:
        print(bnr.simple_run([cmd], catch_error=True).out)

run(["gpr2build", "-Pprj", "-p"])
run(["gpr2build", "-Pprj", "-p"])
run(["./main"])
