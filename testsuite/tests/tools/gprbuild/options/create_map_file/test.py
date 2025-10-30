import os
from e3.fs import rm
from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD

bnr = BuilderAndRunner()

def run(cmd):
    if isinstance (cmd, str):
        print("$ " + cmd);
        args = cmd.split(" ")
    else:
        print("$ " + " ".join(cmd));
        args = cmd
    if args[0] == GPRBUILD:
        bnr.call(args)
    else:
        print(bnr.simple_run(args, catch_error=True).out)

def check(path):
    if os.path.isfile(path):
        print("Ok: " + path)
        rm(path)
    else:
        print("!!! missing the map file in " + path)

run(f"{GPRBUILD} -q prj.gpr --create-map-file")
check("obj/main.map")
run(f"{GPRBUILD} -q prj.gpr --create-map-file=linker_map.map")
check("obj/linker_map.map")
run(f"{GPRBUILD} -q prj1.gpr")
check("obj/main.map")
run(f"{GPRBUILD} -q prj2.gpr")
check("linker_map.map")
run(f"{GPRBUILD} -q prj3.gpr --create-map-file")
check("obj/main.map")
check("obj/main2.map")
run(f"{GPRBUILD} -q prj3.gpr --create-map-file=linker_map.map")
run(f"{GPRBUILD} -q prj3.gpr main2.adb --create-map-file=linker_map.map")
check("obj/linker_map.map")
