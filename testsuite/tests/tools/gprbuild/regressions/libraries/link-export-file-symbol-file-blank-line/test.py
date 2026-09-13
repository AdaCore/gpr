from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD

import os
from pathlib import Path
import stat

bnr = BuilderAndRunner()

def run(cmd):
    print("$ " + " ".join(cmd))
    if cmd[0] in (GPRBUILD):
        bnr.call(cmd)
    else:
        print(bnr.simple_run(cmd, catch_error=True).out)

# "say_hello", a completely blank line (CRLF, zero content -- this is the
# trigger), "say_bye".
with open("symbols.txt", "wb") as f:
    f.write(b"say_hello\r\n")
    f.write(b"\r\n")
    f.write(b"say_bye\r\n")

run([GPRBUILD, "-P", "hello.gpr"])
