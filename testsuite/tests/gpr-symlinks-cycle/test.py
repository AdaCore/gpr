import os
import subprocess

from testsuite_support.builder_and_runner import BuilderAndRunner, GPRCLEAN, GPRINSTALL

bnr = BuilderAndRunner()

bnr.run(
    ["gprbuild", "-eL", os.path.join("project", "prj.gpr")],
    env={"GPR_PROJECT_PATH": os.getcwd()},
)

bnr.run(
    [GPRINSTALL, "-p", "--prefix=" + os.getcwd() + "/inst1", "-r", "project/prj.gpr"],
    env={"GPR_PROJECT_PATH": os.getcwd()},
)

bnr.run(
    [
        GPRINSTALL,
        "-p",
        "--prefix=" + os.getcwd() + "/inst2",
        "-r",
        "-eL",
        "project/prj.gpr",
    ],
    env={"GPR_PROJECT_PATH": os.getcwd()},
)

assert not os.path.exists("inst1"), "inst1 should not have been created"
assert os.path.exists("inst2"), "inst2 should have been created"

assert os.path.exists(
    os.path.join("inst2", "include", "imported_prj_b", "foo.ads")
), "foo.ads should have been installed"

assert os.path.exists(
    os.path.join("inst2", "lib", "imported_prj_b", "foo.ali")
), "foo.ali should have been installed"


assert os.path.exists(
    os.path.join("inst2", "include", "imported_prj_a", "bar.ads")
), "bar.ads should have been installed"

assert os.path.exists(
    os.path.join("inst2", "lib", "imported_prj_a", "bar.ali")
), "bar.ali should have been installed"


bnr.run(
    [GPRCLEAN, "-r", "-eL", os.path.join("project", "prj.gpr")],
    env={"GPR_PROJECT_PATH": os.getcwd()},
)

assert not os.path.exists("import_B/obj/foo.o"), "foo.o should have been removed"
assert not os.path.exists("import_A/obj/bar.o"), "bar.o should have been removed"

assert os.path.exists("import_B/src/foo.ads"), "foo.ads should not have been removed"
assert os.path.exists("import_A/src/bar.ads"), "bar.ads should not have been removed"
