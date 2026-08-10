from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD, GPRCLEAN

bnr = BuilderAndRunner()

# Prj1's main ("main.c") restricts its Roots to ("pkg*"), and Prj2's
# "main2.adb" is neither "with"ed nor a root, so it must never be compiled.
# Prj3 is a "with"ed non-standalone library: its own sources (Toto1/Toto2/
# Toto3) are always compiled to build the library itself regardless of
# Roots, so we don't check compilation for it - we only check that its
# units don't leak into the bind action command line for "main", since they
# are neither "with"ed by Pkg/Pkg2 nor matching the Roots restriction.

p = bnr.simple_run(
    [GPRBUILD, "-P", "tree/prj1/prj1.gpr", "-f", "-v"], catch_error=True
)

compile_lines = [
    line for line in p.out.splitlines() if " -c -x " in line
]

not_compiled = ["main2.adb"]
unexpected_compiles = [
    f for f in not_compiled if any(f in line for line in compile_lines)
]

if unexpected_compiles:
    print(
        "FAILED: unexpected source(s) compiled: "
        + ", ".join(unexpected_compiles)
    )
else:
    print("OK: main2.adb (not a root, not with'ed) was not compiled")

expected_compiled = ["pkg.adb", "pkg2.adb", "main.c"]
missing_compiles = [
    f
    for f in expected_compiled
    if not any(f in line for line in compile_lines)
]

if missing_compiles:
    print(
        "FAILED: expected source(s) not compiled: "
        + ", ".join(missing_compiles)
    )
else:
    print("OK: pkg.adb, pkg2.adb and main.c were compiled")

gnatbind_lines = [line for line in p.out.splitlines() if "gnatbind" in line]

if not gnatbind_lines:
    print("FAILED: no gnatbind invocation found in the build output")
else:
    # There is a single gnatbind call, binding the "main" partition.
    gnatbind_cmd = gnatbind_lines[-1]

    not_expected = ["toto1.ali", "toto2.ali", "toto3.ali"]
    unexpected = [f for f in not_expected if f in gnatbind_cmd]

    if unexpected:
        print(
            "FAILED: gnatbind command line unexpectedly references "
            + ", ".join(unexpected)
            + " (units from Prj3 that are outside the Roots-restricted "
            + "closure of \"main.c\" should not be bound)"
        )
    else:
        print(
            "OK: gnatbind command line does not reference Prj3's "
            "toto1.ali/toto2.ali/toto3.ali"
        )

    expected = ["pkg.ali", "pkg2.ali"]
    missing = [f for f in expected if f not in gnatbind_cmd]

    if missing:
        print(
            "FAILED: gnatbind command line is missing "
            + ", ".join(missing)
        )
    else:
        print("OK: gnatbind command line references pkg.ali and pkg2.ali")
