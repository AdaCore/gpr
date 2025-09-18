import os
from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD
from e3.env import Env


bnr = BuilderAndRunner()

def run(cmd):
    print("$ " + " ".join(cmd));
    if cmd[0] == GPRBUILD:
        bnr.call(cmd)
    else:
        print(bnr.simple_run(cmd, catch_error=True).out)

run ([GPRBUILD, "-q", "-Ptree/agglib.gpr"])

# lookup for Ada.Strings.Unbounded.Null_Unbounded_String in the generated
# library

sym = "ada__strings__unbounded__null_unbounded_string"

if Env().host.os.name == "windows":
    lib = "liblib.dll"
    # easiest way to check exported symbols for a dll is to use gendef that will
    # generate a def file for the library with the list of exported syms
    bnr.simple_run(["gendef", f"tree/lib/{lib}"], catch_error=True)
    with open("liblib.def") as fp:
        cnt = fp.read()
    found = False
    for l in cnt.splitlines():
        if l == sym:
            found = True
    if found:
        print("Ada.Strings.Unbounded exported, wrong!")
    else:
        print("Ada.Strings.Unbounded not exported, good!")

else:
    lib = "liblib.so"
    out = bnr.simple_run(["nm", f"tree/lib/{lib}"], catch_error=True).out


    for l in out.splitlines():
        if l.endswith(f"b {sym}"):
            print("Ada.Strings.Unbounded not exported, good!")
        elif l.endswith(f"B {sym}"):
            print("Ada.Strings.Unbounded exported, wrong!")
