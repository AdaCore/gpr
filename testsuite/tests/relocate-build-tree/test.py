import json
import os
import re
import subprocess

from testsuite_support.builder_and_runner import BuilderAndRunner, GPRINSPECT


bnr = BuilderAndRunner()
test_number = 1


def test(command):
    global test_number
    print("--- " + str(test_number))
    print("$ " + ' '.join(command))
    test_number += 1
    cnt = bnr.run(command).out.splitlines()
    while True:
        if re.match(r"[a-z]+.gpr(:[0-9]+)*: (warning|error): .*", cnt[0]):
            print(cnt[0])
            del(cnt[0])
        else:
            break
    if len(cnt) <= 1:
        return
    cnt = "\n".join(cnt)
    val = json.loads(cnt)
    for prj in val["projects"]:
        name = prj["project"]["name"]
        if name == "runtime":
            continue
        obj_dir = os.path.relpath(prj["project"]["object-directory"])
        print(f"{name}.obj_dir = {obj_dir}")


build_dir = os.path.join(os.getcwd(), 'tmp')
tree_dir = os.path.join(os.getcwd(), 'prj')

if not os.path.exists(build_dir):
    os.mkdir(build_dir)

gprinspect = [GPRINSPECT, "-r", "--display=json"]
gpr = ["-Pprj/prj"]
subgpr = ["-Pprj/subdir/prj"]
root_dir = ["-XTEST_ROOT_DIR=" + os.getcwd() + "/prj"]
relocate_build_tree = ["--relocate-build-tree=tmp"]

test(gprinspect + gpr + root_dir + relocate_build_tree)
test(gprinspect + gpr + root_dir)
test(gprinspect + gpr + relocate_build_tree)
test(gprinspect + gpr)

test(gprinspect + subgpr + relocate_build_tree)
test(gprinspect + subgpr + relocate_build_tree + ["--root-dir=prj"])
