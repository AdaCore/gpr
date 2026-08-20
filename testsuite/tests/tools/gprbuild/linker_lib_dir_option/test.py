"""Check how the linker library directory option is resolved.

The option can be given joined to its path ("-Ldir"), or on its own, its path
being the next value of the list ("-L", "dir"). Both spellings must reach the
linker as an absolute path, a value that is an option itself must never be
turned into a path, and none of the degenerate forms may raise.

Only verdicts are printed, since the command lines hold absolute paths. A
verdict that fails also prints the link command line, with the paths of the
test made relative, as that is what tells why it failed.
"""

import os

from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD

bnr = BuilderAndRunner()

CWD = os.getcwd()

# Directory the -L options of prj.gpr point to, as the linker should see it.
# Path_Name.String_Value drops the trailing directory separator, so the
# resolved option has none either.
LIB_DIR = os.path.join(CWD, "libdir")

CASES = [
    ("joined",
     [("the path is absolute", "-L" + LIB_DIR, True)]),

    ("split",
     [("the option is passed on its own", " -L ", True),
      ("the next value is the absolute path", " -L " + LIB_DIR, True)]),

    ("option_after",
     [("the option without a path is passed as is", " -L -L", True),
      ("the following option is not turned into a path",
       os.path.join(CWD, "-Llibdir"), False),
      ("the following option keeps its own absolute path",
       " -L -L" + LIB_DIR, True)]),

    ("repeated",
     [("a run of options without a path is passed as is", " -L -L -L", True),
      ("none of them is turned into a path",
       os.path.join(CWD, "-L"), False),
      ("the last one keeps its own absolute path",
       " -L -L -L" + LIB_DIR, True)]),

    ("dangling", []),
]


def link_command(out):
    """Return the link command line out of a verbose build log"""
    for line in out.splitlines():
        # Of the commands taking the binder generated unit, the link is the
        # only one that neither compiles it nor produces it
        if ("b__main." in line
            and " -c " not in line
            and "gnatbind" not in line):
            return line

    return None


for case, checks in CASES:
    p = bnr.run([GPRBUILD, "-p", "-v", "-P", "prj.gpr", "-XCASE=" + case])
    cmd = link_command(p.out)

    if "raised " in p.out:
        print("FAIL: " + case + ": an exception was raised")
        continue

    if cmd is None:
        print("FAIL: " + case + ": no link command line was computed")
        continue

    print("OK: " + case + ": the link command line was computed")

    for label, text, expected in checks:
        if (text in p.out) == expected:
            print("OK: " + case + ": " + label)
        else:
            print("FAIL: " + case + ": " + label)
            print("  link command line: " + cmd.replace(CWD, "."))
