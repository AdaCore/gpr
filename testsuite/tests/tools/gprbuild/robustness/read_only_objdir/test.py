import os
from e3.fs import rm
from testsuite_support.builder_and_runner import BuilderAndRunner

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
    if args[0].startswith("gpr2"):
        bnr.call(args)
    else:
        print(bnr.simple_run(args, catch_error=True).out)

print("regular run:")

run("gpr2build -Pprj -p")
run("gpr2clean -Pprj")

print("\nread only obj dir:")

chro("obj/prj")
run("gpr2build -Pprj -p")
run("gpr2clean -Pprj -v")

print("\nread only obj dir with built project")

chrw("obj/prj")
run("gpr2build -Pprj -p -q")
chro("obj/prj")

print("- with externally built dependency")
run("gpr2build -Plib -XPRJ_RO=True -p")

print("- with regular dependency")
run("gpr2build -Plib -XPRJ_RO=False -p")
chrw("obj/prj")

print("\nread only lib dir")
run("gpr2clean -Plib -r -q")
chro("lib/lib")
run("gpr2build -Plib")
chrw("lib/lib")
run("gpr2build -Plib -q")
chro("lib/lib")
run("gpr2build -Plib -f -l")
