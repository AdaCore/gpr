from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD, GPRCLEAN

import os

bnr = BuilderAndRunner()

def run(cmd):
    print("$ " + " ".join(cmd))
    if cmd[0] in (GPRBUILD, GPRCLEAN):
        bnr.call(cmd)
    else:
        print(bnr.simple_run([cmd], catch_error=True).out)

def get_files_by_prefix(directory, prefix):
    print(f"$ ls -l {directory}/{prefix}*")
    all_items = os.listdir(directory)

    matches = []
    for f in all_items:
        full_path = os.path.join(directory, f)

        if f.startswith(prefix) and (os.path.isfile(full_path) or os.path.islink(full_path)):
            target = None
            if os.path.islink(full_path):
                target = os.readlink(full_path)
            
            matches.append((f, target))

    matches.sort(key=lambda x: x[0].lower())

    for name, target in matches:
        if target:
            print(f"{name} -> {target}")
        else:
            print(name)

run([GPRBUILD, "-P", "prj.gpr"])
get_files_by_prefix("lib", "libprj")
