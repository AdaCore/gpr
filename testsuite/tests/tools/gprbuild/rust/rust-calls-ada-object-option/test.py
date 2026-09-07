import json
import os

from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD

bnr = BuilderAndRunner()

bnr.simple_run(
    [
        "gcc",
        "-c",
        os.path.join("tree", "extra_input.c"),
        "-o",
        os.path.join("tree", "rust", "extra_input.o"),
    ],
    analyze_output=False,
)

bnr.simple_run(
    [
        GPRBUILD,
        "-q",
        "-P",
        os.path.join("tree", "main.gpr"),
        "-p",
        "--json-summary",
    ],
    analyze_output=False,
)

with open(os.path.join("tree", "jobs.json")) as fp:
    jobs = json.load(fp)

cargo = [job for job in jobs if job["uid"].startswith("[Cargo-Build]")]

flags = (
    cargo[0].get("environment", {}).get("CARGO_ENCODED_RUSTFLAGS", "").split("\x1f")
)

print("handed over as it stands:", "link-arg=extra_input.o" in flags)
print("turned into an archive:", "-lstatic=extra_input" in flags)
