import os

from e3.env import Env
from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD

bnr = BuilderAndRunner()

# 1. Build the library normally. This produces tree/lib/libmylib.a whose
#    members include the dedicated options object o__mylib.o.
bnr.simple_run([GPRBUILD, "-P" + os.path.join("tree", "lib.gpr"), "-p", "-q"])

archive = os.path.join("tree", "lib", "libmylib.a")

# 2. Fake linker options, terminated by the platform's native line ending:
#    "\n" on Unix, "\r\n" on Windows. This exercises the extraction on both --
#    in particular the carriage return must be stripped from the option on
#    Windows. Written in binary mode so the bytes are exactly as intended.
platform = Env().host.platform
eol = (b"\r\n" if platform.endswith("windows") or platform.endswith("windows64")
       else b"\n")
with open("opts.txt", "wb") as f:
    f.write(b"-lgpr2_fake_marker" + eol)

# 3. Turn the archive into a "gprbuild1" style archive: embed the
#    .GPR.linker_options section into a regular member (pkg1.o) and remove the
#    dedicated o__mylib.o member entirely, so no "o__" member remains.
bnr.call(["ar", "x", archive, "pkg1.o"])
bnr.call(["objcopy", "--add-section", ".GPR.linker_options=opts.txt",
          "pkg1.o", "pkg1_opts.o"])
os.replace("pkg1_opts.o", "pkg1.o")
bnr.call(["ar", "r", archive, "pkg1.o"])
bnr.call(["ar", "d", archive, "o__mylib.o"])

# Sanity: the archive must no longer contain any o__ member.
members = bnr.simple_run(["ar", "t", archive]).out
if any(line.strip().startswith("o__") for line in members.splitlines()):
    print("ERROR: the archive still contains an o__ member")

# 4. Build and run the driver.
bnr.build("test.gpr", args=["-p", "-q"])
bnr.call([os.path.join("obj", "test")])
