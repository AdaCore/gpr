from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD, GPRCLEAN
import re

bnr = BuilderAndRunner()

def run(cmd):
    print("$ " + " ".join(cmd))
    if cmd[0] in (GPRBUILD, GPRCLEAN):
        bnr.call(cmd)
    else:
        print(bnr.simple_run(cmd, catch_error=True).out)

def normalize_ldd(output: str) -> str:
    lines = []
    for line in output.splitlines():
        line = line.strip()
        if not line:
            continue
        # drop the trailing "(0x00007f...)" address
        line = re.sub(r'\s*\(0x[0-9a-fA-F]+\)\s*$', '', line)
        # drop the "=> /full/path/to/lib" part, keep only the name before it
        line = re.sub(r'\s*=>\s*\S+', '', line)
        # bare path entries (e.g. /lib64/ld-linux-x86-64.so.2)
        if line.startswith('/'):
            line = line.rsplit('/', 1)[-1]
        # drop the version suffix after .so (e.g. libc.so.6 -> libc.so, ld-linux-x86-64.so.2 -> ld-linux-x86-64.so)
        line = re.sub(r'(\.so)(\.\d+)+$', r'\1', line)
        lines.append(line)
    return '\n'.join(sorted(lines))

run([GPRBUILD, "-P", "tree/p.gpr", "-j1"])
print("$ ldd tree/obj/p/main")
print(normalize_ldd(bnr.simple_run(["ldd", "tree/obj/p/main"]).out))
