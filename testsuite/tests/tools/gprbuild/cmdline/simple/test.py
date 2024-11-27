import os
from e3.fs import rm
from testsuite_support.builder_and_runner import BuilderAndRunner

bnr = BuilderAndRunner()

def cleanup():
    for sub in "ada", "c", "mixed", "multi":
        rm(sub + "/main")
        rm(sub + "/hello")
        rm(sub + "/*.o")
        rm(sub + "/*.ali")
        rm(sub + "/b__*")
        rm(sub + "/.*.json")

def run(cmd, cwd=""):
    if cwd != "":
        old_cwd = os.getcwd()
        os.chdir(cwd)
    if isinstance (cmd, str):
        print("$ " + cmd);
    else:
        print("$ " + " ".join(cmd));
    if cmd[0] == "gpr2build":
        bnr.call(cmd)
    else:
        print(bnr.simple_run([cmd], catch_error=True).out)
    if cwd != "":
        os.chdir(old_cwd)

run(["gpr2build", "main.adb", "-q"], "ada")
run(["ada/main"])
cleanup()
run(["gpr2build", "main", "-q"], "ada")
run(["ada/main"])
cleanup()
run(["gpr2build", "hello.c", "-q", "-o", "main"], "c")
run(["c/main"])
cleanup()
run(["gpr2build", "hello.c", "main.adb", "-q"], "mixed")
run(["mixed/main"])
run(["mixed/hello"])
cleanup()
run(["gpr2build", "-Pmulti", "src.ada", "-eI1", "-o", "main", "-q"], "multi")
run(["multi/main"])
run(["gpr2build", "-Pmulti", "src.ada", "-eI2", "-o", "main", "-q"], "multi")
run(["multi/main"])
run(["gpr2build", "-Pmulti", "src.ada", "-eI3", "-o", "main", "-q"], "multi")
run(["multi/main"])
cleanup()
# erroneous cases with indexing
run(["gpr2build", "hello.c", "-eI1"], "c")
run(["gpr2build", "main.adb", "-eI1"], "ada")
run(["gpr2build", "non_existing.adb"], "ada")
run(["gpr2build", "main.adb", "hello.c", "-eI1"], "mixed")
run(["gpr2build", "non_existing.adb"], "ada")
run(["gpr2build", "-Pmulti", "src.ada", "-eI4", "-o", "main"], "multi")
run(["gpr2build", "-Pmulti", "src.ada", "-o", "p4"], "multi")
