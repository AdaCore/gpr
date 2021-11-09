import os
import subprocess

from testsuite_support.builder_and_runner import BuilderAndRunner, GPRCLEAN


bnr = BuilderAndRunner()
test_number = 1


def test(gprbuild_command, gprclean_command):
    global test_number
    print("--- " + str(test_number))
    test_number += 1
    subprocess.check_output(gprbuild_command, shell=True)
    print(bnr.run(gprclean_command).out)


root_dir = "-XTEST_ROOT_DIR=" + os.getcwd() + "/prj/obj/"
relocate_build_tree = "--relocate-build-tree=" + os.getcwd() + "/prj/lib/"

test(
    "gprbuild -p -q -Pprj/prj " + root_dir + " " + relocate_build_tree,
    [GPRCLEAN, "-r", "-Pprj/prj", root_dir, relocate_build_tree],
)

test("gprbuild -p -q -Pprj/prj " + root_dir, [GPRCLEAN, "-r", "-Pprj/prj", root_dir])

test(
    "gprbuild -p -q -Pprj/prj " + relocate_build_tree,
    [GPRCLEAN, "-r", "-Pprj/prj", relocate_build_tree],
)

test("gprbuild -p -q -Pprj/prj", [GPRCLEAN, "-r", "-Pprj/prj"])
