import glob
import os

from e3.env import Env
from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD, GPRCLEAN

bnr = BuilderAndRunner()

exe = ".exe" if "windows" in Env().host.platform else ""
prj = os.path.join("tree", "wrapped.gpr")
target = os.path.join("tree", "rust", "target")


def artifact(name):
    return glob.glob(os.path.join(target, "*", "release", name + exe))


def status(name):
    return "found" if artifact(name) else "missing"


#  Only Wrapped is a GPR project; Neighbour is a plain workspace member.
bnr.check_output([GPRBUILD, "-q", "-p", "-P", prj])

#  Build Neighbour into the very directory gprclean is about to clean. The
#  triple comes from where Cargo just put Wrapped's binary, so that both
#  binaries share one target directory.
release = os.path.dirname(artifact("wrapped")[0])
triple = os.path.basename(os.path.dirname(release))
bnr.check_output(
    [
        "cargo",
        "build",
        "-q",
        "-p",
        "neighbour",
        "--manifest-path",
        os.path.join("tree", "rust", "Cargo.toml"),
        "--target",
        triple,
        "--release",
    ]
)

print("wrapped after build:  ", status("wrapped"))
print("neighbour after build:", status("neighbour"))

bnr.check_output([GPRCLEAN, "-P", prj])

print("wrapped after clean:  ", status("wrapped"))
print("neighbour after clean:", status("neighbour"))
