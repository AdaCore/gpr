import os.path
from e3.env import Env
from e3.os.process import Run

from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD

bnr = BuilderAndRunner()

# Build the custom Ada executable that Put_Lines several lines.
Run([GPRBUILD, "-p", "-q", os.path.join("printer", "printer.gpr")])

bnr.build("test.gpr", args=["-p", "-q"])
out = bnr.call(["./test"], quiet=True).raw_out

# Check the relayed line terminators: native CR/LF on Windows, bare LF on Unix.
# Output should be: '"obj" created/r/n[Multiline]       multiline/r/nline 1/r/nline 2/r/nline 3/r/n'

if "windows" in Env().host.platform:
    count = out.count(b"\r\n")
else:
    count = out.count(b"\n")

# A stray CR is a '\r' not immediately followed by a '\n'. We shouldn't have
# any, no matter the env.
stray_cr = out.count(b"\r") - out.count(b"\r\n")

if count == 5 and stray_cr == 0:
    print("Ok")
else:
    print(f"Invalid line separators (count={count}, stray_cr={stray_cr})."
          f" Output was: {out}")
