import os
from e3.fs import rm
from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD, GPRCLEAN

bnr = BuilderAndRunner()

def chro(path):
    print ("$ chmod 0x555 " + path)
    os.chmod(path, 0o555)

def chrw(path):
    print ("$ chmod 0x777 " + path)
    os.chmod(path, 0o777)

def run(cmd):
    if isinstance (cmd, str):
        print("$ " + cmd);
        args = cmd.split(" ")
    else:
        print("$ " + " ".join(cmd));
        args = cmd
    if args[0] in (GPRBUILD, GPRCLEAN):
        bnr.call(args)
    else:
        print(bnr.simple_run(args, catch_error=True).out)

print("regular run:")

run(f"{GPRBUILD} -Pprj -p -j1 --temp-dir=obj")
run(f"{GPRCLEAN} -Pprj")

print("\nread only obj dir:")

chro("obj/prj")
run(f"{GPRBUILD} -Pprj -p -j1 --temp-dir=obj")
run(f"{GPRCLEAN} -Pprj -v")

print("\nread only obj dir with built project")

chrw("obj/prj")
run(f"{GPRBUILD} -Pprj -p -q --temp-dir=obj")
chro("obj/prj")

print("- with externally built dependency")
run(f"{GPRBUILD} -Plib -XPRJ_RO=True -p -j1 --temp-dir=obj")

print("- with regular dependency")
run(f"{GPRBUILD} -Plib -XPRJ_RO=False -p -j1 --temp-dir=obj")
chrw("obj/prj")

print("\nread only lib dir")
run(f"{GPRCLEAN} -Plib -r -q")
chro("lib/lib")
run(f"{GPRBUILD} -Plib -j1 --temp-dir=obj")
chrw("lib/lib")
run(f"{GPRBUILD} -Plib -q --temp-dir=obj")
chro("lib/lib")
run(f"{GPRBUILD} -Plib -f -j1 --temp-dir=obj")
