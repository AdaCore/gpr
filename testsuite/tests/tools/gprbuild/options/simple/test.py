import os
from e3.fs import rm
from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPR2BUILD

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
        args = cmd.split(" ")
    else:
        print("$ " + " ".join(cmd));
    if args[0] == GPR2BUILD:
        bnr.call(args)
    else:
        print(bnr.simple_run(args, catch_error=True).out)
    if cwd != "":
        os.chdir(old_cwd)

run(f"{GPR2BUILD} main.adb -q", "ada")
run("ada/main")
cleanup()
run(f"{GPR2BUILD} main -q", "ada")
run("ada/main")
cleanup()
run(f"{GPR2BUILD} hello.c -q -o main", "c")
run("c/main")
cleanup()
run(f"{GPR2BUILD} hello.c main.adb -q", "mixed")
run("mixed/main")
run("mixed/hello")
cleanup()
run(f"{GPR2BUILD} -Pmulti src.ada -eI1 -o main -q", "multi")
run("multi/main")
run(f"{GPR2BUILD} -Pmulti src.ada -eI2 -o main -q", "multi")
run("multi/main")
run(f"{GPR2BUILD} -Pmulti src.ada -eI3 -o main -q", "multi")
run("multi/main")
cleanup()
# erroneous cases with indexing
run(f"{GPR2BUILD} hello.c -eI1 -j1", "c")
run(f"{GPR2BUILD} main.adb -eI1 -j1", "ada")
run(f"{GPR2BUILD} non_existing.adb -j1", "ada")
run(f"{GPR2BUILD} main.adb hello.c -eI1 -j1", "mixed")
run(f"{GPR2BUILD} non_existing.adb -j1", "ada")
run(f"{GPR2BUILD} -Pmulti src.ada -eI4 -o main -j1", "multi")
run(f"{GPR2BUILD} -Pmulti src.ada -o p4 -j1", "multi")
# erroneous case of mains with a library
run(f"{GPR2BUILD} -Plib main.adb -j1")
