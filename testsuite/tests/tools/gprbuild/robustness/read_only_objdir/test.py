import os
from e3.fs import rm
from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPR2BUILD, GPR2CLEAN

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
    if args[0] in (GPR2BUILD, GPR2CLEAN):
        bnr.call(args)
    else:
        print(bnr.simple_run(args, catch_error=True).out)

print("regular run:")

run(f"{GPR2BUILD} -Pprj -p -j1")
run(f"{GPR2CLEAN} -Pprj")

print("\nread only obj dir:")

chro("obj/prj")
run(f"{GPR2BUILD} -Pprj -p -j1")
run(f"{GPR2CLEAN} -Pprj -v")

print("\nread only obj dir with built project")

chrw("obj/prj")
run(f"{GPR2BUILD} -Pprj -p -q")
chro("obj/prj")

print("- with externally built dependency")
run(f"{GPR2BUILD} -Plib -XPRJ_RO=True -p -j1")

print("- with regular dependency")
run(f"{GPR2BUILD} -Plib -XPRJ_RO=False -p -j1")
chrw("obj/prj")

print("\nread only lib dir")
run(f"{GPR2CLEAN} -Plib -r -q")
chro("lib/lib")
run(f"{GPR2BUILD} -Plib -j1")
chrw("lib/lib")
run(f"{GPR2BUILD} -Plib -q")
chro("lib/lib")
run(f"{GPR2BUILD} -Plib -f -j1")
