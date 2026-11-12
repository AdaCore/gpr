import json
from pathlib import Path
import shutil

from e3.env import Env
from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD


bnr = BuilderAndRunner()
fifo = "--jobserver-auth=fifo:dummy"
force_jobserver = ["--autodetect-jobserver"]
diagnostic = (
    "error: jobserver fifo protocol not supported, please use "
    "--jobserver-style=pipe on your make command"
)

# Each case gets a fresh project so a previous build cannot mask an early
# scheduler return. No real FIFO or particular version of make is needed.
cases = [
    ("forced_fifo", fifo, force_jobserver, 7, False),
    ("forced_fifo_json", fifo, force_jobserver + ["--json-summary"], 7, False),
    ("fifo_fallback", fifo, [], 0, True),
    ("no_jobserver", "", force_jobserver, 0, True),
    ("dry_run", "n " + fifo, force_jobserver, 0, False),
]

for name, makeflags, switches, expected_status, expected_executable in cases:
    directory = Path(name)
    directory.mkdir()
    for filename in ("hello.gpr", "hello.adb"):
        shutil.copy2(filename, directory / filename)

    result = bnr.run(
        [GPRBUILD, "-P" + str(directory / "hello.gpr"), "-p"] + switches,
        env={"MAKEFLAGS": makeflags},
    )
    assert result.status == expected_status, (
        f"{name}: expected exit {expected_status}, got {result.status}\n"
        + result.out
    )
    assert result.out.count(diagnostic) == (expected_status == 7), result.out

    executable = directory / "bin" / ("hello" + Env().host.os.exeext)
    assert executable.exists() == expected_executable, name

    if "--json-summary" in switches:
        assert json.loads((directory / "jobs.json").read_text()) == []

    print(f"{name}: exit {result.status}, executable {executable.exists()}")
