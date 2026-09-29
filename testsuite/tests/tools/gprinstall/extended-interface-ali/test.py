import os

from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD, GPRINSTALL

bnr = BuilderAndRunner()

prefix = os.path.join(os.getcwd(), "inst")

#  Hidden is withed by Pkg, the only declared interface unit, so the bind
#  adds it to the interface: this is what the warning below reports.
bnr.call([GPRBUILD, "-p", "-q", "-P", "lib.gpr"])

print(bnr.run([GPRINSTALL, "-p", "-P", "lib.gpr", "--prefix=" + prefix]).out)

#  Hidden's ALI is installed like the other interface units...
for root, _, names in os.walk(prefix):
    for name in sorted(names):
        if name.endswith(".ali"):
            print(name)

#  ...and the installed project declares it, so that its interface matches
#  what sits beside it.
with open(os.path.join(prefix, "share", "gpr", "lib.gpr")) as f:
    for line in f:
        if "Library_Interface" in line:
            print(line.strip())

#  A library built against the installed one must in turn be installable:
#  this is what fails when the declared interface is narrower than the
#  installed closure.
os.environ["GPR_PROJECT_PATH"] = os.pathsep.join(
    [os.path.join(prefix, "share", "gpr"), os.environ.get("GPR_PROJECT_PATH", "")]
)

os.chdir("consumer")
bnr.call([GPRBUILD, "-p", "-q", "-P", "app.gpr"])
print(bnr.run([GPRINSTALL, "-p", "-P", "app.gpr", "--prefix=" + prefix]).out)
