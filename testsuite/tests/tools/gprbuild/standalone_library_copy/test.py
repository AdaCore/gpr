import os

from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD

bnr = BuilderAndRunner()


def listing(label, directory, suffixes):
    print("$ ls " + label)
    for name in sorted(os.listdir(directory)):
        if name.endswith(suffixes):
            print(name)


# Building the main runs the library copy action in a worker task while the
# main task keeps scheduling and collecting the other actions. That action
# works on the snapshot Pre_Execution takes of the tree and of the view, so
# check the files it produces: a wrong or missing snapshot would silently
# copy the wrong set.

bnr.call([GPRBUILD, "-P", os.path.join("tree", "main", "main.gpr"), "-p", "-q"])

# The ALI of every interface unit, and of no other unit, is copied to the
# Library_ALI_Dir. This pins the Dependency_File snapshot.

listing("tree/ali", os.path.join("tree", "ali"), (".ali",))

# Only the spec of an interface unit is copied, unless the ALI marks it as
# needing its body (Body_Needed_For_SAL): "fast" is inlined so both its parts
# are copied, "greeter" is not so its body stays out, and "config" has no body
# at all. This pins the Spec_Needs_Body snapshot. "helper" is not part of the
# interface and is not copied.

listing("tree/libsrc", os.path.join("tree", "libsrc"), (".ads", ".adb"))
