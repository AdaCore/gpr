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

def check_no_dynamic_libgcc(binary_path: str) -> str:
    """Return a single deterministic status line: whether libgcc is pulled in
    as a dynamic dependency. Ignores the arch-specific dynamic linker and
    vDSO entries entirely, since their naming varies by arch/ABI and is not
    relevant to what this test checks."""
    result = bnr.simple_run(["ldd", binary_path], catch_error=True)
    output = result.out

    if 'not a dynamic executable' in output:
        return "libgcc_s: static-or-absent (fully static executable)"

    libs = set()
    for line in output.splitlines():
        line = line.strip()
        if not line:
            continue
        name = line.split('=>')[0].strip()
        if name.startswith('/'):
            name = name.rsplit('/', 1)[-1]
        libs.add(name)

    if any(re.match(r'^libgcc_s', lib) for lib in libs):
        return f"libgcc_s: dynamic ({[l for l in libs if l.startswith('libgcc_s')]})"
    return "libgcc_s: static-or-absent"

run([GPRBUILD, "-P", "tree/p.gpr", "-j1"])
print("$ check-static-libgcc tree/obj/p/main")
print(check_no_dynamic_libgcc("tree/obj/p/main"))
