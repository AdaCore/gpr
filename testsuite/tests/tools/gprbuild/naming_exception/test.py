from testsuite_support.builder_and_runner import BuilderAndRunner, GPR2BUILD

bnr = BuilderAndRunner()

def run(cmd):
    print("$ " + " ".join(cmd));
    if cmd[0] == GPR2BUILD:
        bnr.call(cmd)
    else:
        print(bnr.simple_run([cmd], catch_error=True).out)

run([GPR2BUILD, "-Pprj", "-p"])
run([GPR2BUILD, "-Pprj", "-p"])
run(["./main"])
