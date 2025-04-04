import os
from e3.fs import rm
from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPR2BUILD

bnr = BuilderAndRunner()

def run(cmd):
    if isinstance (cmd, str):
        print("$ " + cmd);
        args = cmd.split(" ")
    else:
        print("$ " + " ".join(cmd));
        args = cmd
    if args[0] == GPR2BUILD:
        bnr.call(args)
    else:
        print(bnr.simple_run(args, catch_error=True).out)

def check(path):
    if os.path.isfile(path):
        print("Ok: " + path)
        rm(path)
    else:
        print("!!! missing the map file in " + path)

run(f"{GPR2BUILD} -q prj.gpr --create-map-file")
check("main.map")
run(f"{GPR2BUILD} -q prj.gpr --create-map-file=linker_map.map")
check("linker_map.map")
run(f"{GPR2BUILD} -q prj1.gpr")
check("main.map")
run(f"{GPR2BUILD} -q prj2.gpr")
check("linker_map.map")
run(f"{GPR2BUILD} -q prj3.gpr --create-map-file")
check("main.map")
check("main2.map")
run(f"{GPR2BUILD} -q prj3.gpr --create-map-file=linker_map.map")
run(f"{GPR2BUILD} -q prj3.gpr main2.adb --create-map-file=linker_map.map")
check("linker_map.map")
