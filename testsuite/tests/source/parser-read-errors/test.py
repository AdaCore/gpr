import os
from pathlib import Path

from testsuite_support.builder_and_runner import BuilderAndRunner

bnr = BuilderAndRunner()
bnr.build("test.gpr", args=["-p"])


def check(mode, filename):
    for state in ("unparsed", "parsed"):
        bnr.run(["./main", mode, filename, state], output="run.out")
        assert Path("run.out").read_text() == "unparsed\n", (mode, filename)


for mode in ("use_failure", "device_failure", "program_failure",
             "bad_encoding"):
    check(mode, "unreadable.adb")

check("mapped", "missing.adb")
Path("directory.adb").mkdir()
check("mapped", "directory.adb")

if os.name == "posix" and os.geteuid() != 0:
    path = Path("unreadable.adb")
    path.write_text("procedure Unreadable is begin null; end Unreadable;")
    permissions = path.stat().st_mode
    path.chmod(0)
    try:
        check("mapped", str(path))
    finally:
        path.chmod(permissions)

print("read failures handled")
