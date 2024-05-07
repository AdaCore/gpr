import os
import subprocess

from testsuite_support.builder_and_runner import BuilderAndRunner, GPRCLEAN

bnr = BuilderAndRunner()

bnr.run(
    ["gprbuild", "-p", os.path.join("A", "example1.gpr")],
    env={"GPR_PROJECT_PATH": os.getcwd()},
)

assert os.path.exists(os.path.join("A", "obj")), "obj for A should have been created"
assert os.path.exists(os.path.join("B", "obj")), "obj for B should have been created"

assert os.path.exists(
    os.path.join("A", "obj", "example1_main.o")
), "example1_main.o not found"
assert os.path.exists(
    os.path.join("B", "obj", "example2_data.o")
), "example2_data.o not found"

assert os.path.exists(
    os.path.join("A", "obj", "example1_main")
), "example1_main not found"

bnr.run(
    [GPRCLEAN, "-r", "-p", os.path.join("A", "example1.gpr")],
    env={"GPR_PROJECT_PATH": os.getcwd()},
)

assert not os.path.exists(
    os.path.join("A", "obj")
), "obj for A should have been cleaned"
assert not os.path.exists(
    os.path.join("B", "obj")
), "obj for B should have been cleaned"
