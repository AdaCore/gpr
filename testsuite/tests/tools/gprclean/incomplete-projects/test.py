import os

from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD, GPRCLEAN

bnr = BuilderAndRunner()

SRC_SUBDIR = "obj/extra_src"


def state(path):
    """Report whether an artifact of the tree is still there."""
    status = "present" if os.path.exists(os.path.join("tree", path)) else "absent"
    print(f"{path}: {status}")


def leftovers():
    """List what is left in the build directories, sources excluded."""
    found = []

    for top in ("obj", "lib"):
        for dirpath, _, filenames in os.walk(os.path.join("tree", top)):
            for name in filenames:
                path = os.path.relpath(os.path.join(dirpath, name), "tree")
                path = path.replace(os.sep, "/")

                if not path.startswith(SRC_SUBDIR + "/"):
                    found.append(path)

    for path in sorted(found):
        print("left over: " + path)


# gprbuild requires extra_src to build correctly

bnr.check_output(
    [GPRBUILD, "-p", "-q", "-Ptree/lib.gpr", "--src-subdirs=extra_src"]
)

# Clean without --src-subdirs: the tree will be incomplete, but the ALI files
# left over by gprbuild still refer to the Extra unit. We should not crash
# on this case, and only clean what is refered in the tree.

p = bnr.run([GPRCLEAN, "-p", "-q", "-Ptree/lib.gpr"])
print(p.out, end="")
print("gprclean status:", p.status)

state("obj/iface.ali")
state("obj/extra.ali")

# Cleaning with the very same switches as the build now removes everything,
# including the artifacts the previous invocation had to skip.

p = bnr.run([GPRCLEAN, "-p", "-q", "-Ptree/lib.gpr", "--src-subdirs=extra_src"])
print(p.out, end="")
print("gprclean --src-subdirs status:", p.status)

state("obj/extra.ali")
leftovers()
