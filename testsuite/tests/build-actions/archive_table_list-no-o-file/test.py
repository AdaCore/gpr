import os

from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD

bnr = BuilderAndRunner()

# 1. Build the library normally. This produces tree/lib/libmylib.a whose
#    members include the dedicated options object o__mylib.o.
bnr.simple_run([GPRBUILD, "-P" + os.path.join("tree", "lib.gpr"), "-p", "-q"])

archive = os.path.join("tree", "lib", "libmylib.a")

# 2. Fake linker options, in the same format link-options-insert writes them
#    (one option per line, no trailing newline).
with open("opts.txt", "w") as f:
    f.write("-lgpr2_fake_marker")

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
