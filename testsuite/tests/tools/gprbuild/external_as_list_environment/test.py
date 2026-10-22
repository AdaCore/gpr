import json

from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD


bnr = BuilderAndRunner()


def compile(label, value, extra_args=()):
    bnr.simple_run(
        [GPRBUILD, "-P", "demo.gpr", "-p", "-q", "-f", "-c", "--json-summary"]
        + list(extra_args),
        env={"GNATMAKE_SWITCHES": value},
        analyze_output=False,
    )
    with open("jobs.json") as summary:
        jobs = json.load(summary)
    commands = [
        job["command"].split()
        for job in jobs
        if job["uid"].startswith("[Ada Compile]")
    ]
    assert len(commands) == 1, commands
    switches = [arg for arg in commands[0] if arg in ("-O0", "-O2", "-g")]
    print(label + ": " + repr(switches))


compile("Environment", "-O0 -g")
compile("Command line", "-O0 -g", ["-XGNATMAKE_SWITCHES=-O2"])
compile("Empty command line", "-O0 -g", ["-XGNATMAKE_SWITCHES="])
compile("Empty environment", "")
