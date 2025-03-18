from testsuite_support.builder_and_runner import BuilderAndRunner

bnr = BuilderAndRunner()

def run(cmd):
    print("$ " + " ".join(cmd));
    if cmd[0] == "gpr2build":
        out = bnr.check_output(cmd).out
    else:
        out = bnr.simple_run([cmd], catch_error=True).out
    print('\n'.join(sorted(out.splitlines())))

run(["gpr2build", "-p", "-Pprj", "-j1"])
run(["gpr2build", "-p", "-Pext", "-j1"])
run(["./main"])
run(["gpr2build", "-p", "-Pext2", "-j1"])
run(["./main"])
