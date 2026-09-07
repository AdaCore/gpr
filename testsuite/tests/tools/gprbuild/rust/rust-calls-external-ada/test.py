import glob
import json
import os

from e3.env import Env
from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD

bnr = BuilderAndRunner()

exe = ".exe" if "windows" in Env().host.platform else ""

#  Build the library first: what follows uses it as an already built one,
#  so the options its binder recorded are only in the archive.

bnr.call(
    [GPRBUILD, "-q", "-P", os.path.join("tree", "provider", "math_lib.gpr"), "-p"]
)

bnr.call(
    [
        GPRBUILD,
        "-q",
        "-P",
        os.path.join("tree", "main.gpr"),
        "-p",
        "--json-summary",
    ]
)

matches = glob.glob(
    os.path.join("tree", "rust", "target", "*", "release", "main_from_rust" + exe)
)
print(bnr.simple_run([matches[0]], catch_error=True).out)

#  What Cargo was handed. The flags do not appear on its command line: Cargo
#  reads them from CARGO_ENCODED_RUSTFLAGS, one flag per 0x1f separated field.
with open(os.path.join("tree", "jobs.json")) as fp:
    jobs = json.load(fp)

cargo = [job for job in jobs if job["uid"].startswith("[Cargo-Build]")]

print("cargo build jobs:", len(cargo))
print(
    "archive listed:",
    len([job for job in jobs if job["uid"].startswith("[Archive-Table-List]")]),
)

flags = (
    cargo[0].get("environment", {}).get("CARGO_ENCODED_RUSTFLAGS", "").split("\x1f")
)

#  The options of the target, added as soon as a library built by GPR takes
#  part in the link Cargo arranges. A library name is handed to rustc as it
#  stands, while anything else goes through "-C link-arg=", rustc having no
#  option of its own for it.
if "windows" in Env().host.platform:
    target_options = [
        "-l" + name
        for name in (
            "advapi32",
            "bcrypt",
            "gcc",
            "gcc_eh",
            "kernel32",
            "mingw32",
            "mingwex",
            "msvcrt",
            "ntdll",
            "user32",
            "userenv",
            "ws2_32",
        )
    ]
else:
    target_options = ["link-arg=-pthread"]

lib_dir = os.path.abspath(os.path.join("tree", "provider", "lib"))
searched = [os.path.abspath(flag[2:]) for flag in flags if flag.startswith("-L")]

print("the library directory handed to Cargo:", lib_dir in searched)
print("the Ada library handed as an archive:", "-lstatic=mathlib" in flags)
print("the binder option handed to Cargo:", "-lm" in flags)
print(
    "the target link options handed to Cargo:",
    all(option in flags for option in target_options),
)

#  An archive is scanned once, so what resolves it must come after it.
if "-lstatic=mathlib" in flags and all(o in flags for o in target_options):
    print(
        "the archive comes before the target libraries:",
        flags.index("-lstatic=mathlib")
        < min(flags.index(option) for option in target_options),
    )
