import json
import os
import re
import subprocess

from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRINSPECT


bnr = BuilderAndRunner()
test_number = 1


def test(command):
    global test_number
    print("--- " + str(test_number))
    print("$ " + " ".join(command))
    test_number += 1
    cnt = bnr.run(command).out.splitlines()

    # Remove the plain text gprinspect error if it exist, so we can
    # load the JSON afterwards.

    error = False
    for line in cnt:
        if re.match("gprinspect: .*", line):
            print(line)
            cnt.remove(line)
            error = True

    cnt = "\n".join(cnt)
    try:
        val = json.loads(cnt)
    except:
        print(cnt)
        raise
    for conf_msg in val["messages"]["configuration"]:
        print(conf_msg)

    for tree_msg in val["messages"]["tree"]:
        print(tree_msg)

    if error:
        # Do not try to parse the JSON, as it does not contain the "projects"
        # field, but only the "messages" field.
        return

    for prj in val["projects"]:
        name = prj["project"]["name"]
        if name == "runtime":
            continue
        obj_dir = os.path.relpath(prj["project"]["object-directory"])
        print(f"{name}.obj_dir = {obj_dir}")


build_dir = os.path.join(os.getcwd(), "tmp")
tree_dir = os.path.join(os.getcwd(), "prj")

if not os.path.exists(build_dir):
    os.mkdir(build_dir)

gprinspect = [GPRINSPECT, "-r", "--display=json"]
gpr = ["-Pprj/prj"]
subgpr = ["-Pprj/subdir/prj"]
root_dir = ["-XTEST_ROOT_DIR=" + os.getcwd() + "/prj"]
ext_built = ["-XLIB_EXTERNALLY_BUILT=True"]
relocate_build_tree = ["--relocate-build-tree=tmp"]

test(gprinspect + gpr + root_dir + relocate_build_tree)
test(gprinspect + gpr + root_dir)
test(gprinspect + gpr + relocate_build_tree)
test(gprinspect + gpr + ext_built + relocate_build_tree)
test(gprinspect + gpr)

test(gprinspect + subgpr + relocate_build_tree)
test(gprinspect + subgpr + ext_built + relocate_build_tree)
test(gprinspect + subgpr + relocate_build_tree + ["--root-dir=prj"])
