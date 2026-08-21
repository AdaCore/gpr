import os.path

from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD

bnr = BuilderAndRunner()

# "main.adb" is the project's declared Main. "bla" is an
# additional Ada entry point reachable only through the Roots attribute
# (it is not "with"ed by main) and its unit name sorts lexically
# *before* "main".
#
# GNATbind treats the first library-level procedure it is given on its
# command line as the Ada main. GPR2 must therefore always emit the
# declared Main's ALI first when calling gnatbind, no matter what order
# the Roots units happen to be gathered in internally. If it doesn't, the
# build below either fails to link (undefined reference to the Roots
# unit's "_ada_..." symbol) or silently produces an executable whose
# entry point is not the one the project declared.

p = bnr.simple_run(
    [GPRBUILD, "-P", os.path.join("tree", "prj.gpr"), "-f", "-v"],
    catch_error=False,
)

if p.status:
    print("FAILED: gprbuild failed (status " + str(p.status) + ")")
else:
    print("OK: gprbuild succeeded")

gnatbind_lines = [line for line in p.out.splitlines() if "gnatbind" in line]

if not gnatbind_lines:
    print("FAILED: no gnatbind invocation found in the build output")
else:
    # There is a single gnatbind call, binding the "main" partition.
    gnatbind_cmd = gnatbind_lines[-1]

    main_idx = gnatbind_cmd.find("main.ali")
    root_idx = gnatbind_cmd.find("bla.ali")

    if main_idx == -1 or root_idx == -1:
        print(
            "FAILED: gnatbind command line is missing main.ali or "
            "bla.ali"
        )
    elif main_idx < root_idx:
        print(
            "OK: main.ali (the declared Main) is listed before "
            "bla.ali on the gnatbind command line"
        )
    else:
        print(
            "FAILED: bla.ali is listed before main.ali on "
            "the gnatbind command line: GNATbind will select "
            '"bla" as the Ada main instead of the declared Main'
        )

# Cross-check against the generated binder body itself: GNATbind records
# the name of the unit it actually selected as the Ada main in
# "b__main.adb"'s "Ada_Main_Program" pragma Import. This is the most
# direct evidence of which unit ended up being the program's entry point.

binder_body = os.path.join("tree", "obj", "b__main.adb")

if not os.path.exists(binder_body):
    print("FAILED: " + binder_body + " was not generated")
else:
    with open(binder_body) as f:
        content = f.read()

    if '"_ada_main"' in content:
        print(
            'OK: b__main.adb imports "_ada_main" as the '
            "Ada_Main_Program"
        )
    elif '"_ada_bla"' in content:
        print(
            'FAILED: b__main.adb imports "_ada_bla" as '
            "the Ada_Main_Program instead of \"_ada_main\""
        )
    else:
        print(
            "FAILED: could not find an Ada_Main_Program pragma Import in "
            + binder_body
        )