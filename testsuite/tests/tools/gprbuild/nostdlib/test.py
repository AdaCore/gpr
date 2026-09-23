import json
import os

from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD

bnr = BuilderAndRunner()

#  -nostdlib can be handed to gprbuild three ways:
#    - "-largs -nostdlib"  -> Linker class only
#    - "-bargs -nostdlib"  -> Binder class only
#    - bare "-nostdlib"    -> both Linker and Binder (same as
#                             Builder'Switches("Ada"), which is fed through
#                             the very same switch dispatcher)

NOSTDLIB_SCOPES = {
    "none": [],
    "linker": ["-largs", "-nostdlib"],
    "binder": ["-bargs", "-nostdlib"],
    "builder": ["-nostdlib"],
}


def build(project, extra_args, xvars=None):
    """Run gprbuild and return the jobs decoded from --json-summary's
    jobs.json, or an empty list if the build failed before producing one.
    """
    args = ["-q", "-p", "-f", "-P" + project, "--json-summary"]

    for name, value in sorted((xvars or {}).items()):
        args.append("-X{}={}".format(name, value))

    args += extra_args

    print("$ gprbuild " + " ".join(args))
    bnr.simple_run([GPRBUILD] + args, catch_error=False, analyze_output=False)

    jobs = []
    if os.path.exists("jobs.json"):
        try:
            with open("jobs.json") as fp:
                jobs = json.load(fp)
        except ValueError:
            jobs = []
        os.remove("jobs.json")

    return jobs


def jobs_of(jobs, kind):
    return [job for job in jobs if job["uid"].startswith(kind)]


def command_of(job):
    return job.get("command", "").split()


def has_nostdlib(cmd):
    return "-nostdlib" in cmd


def links_runtime(cmd):
    """Whether the GNAT runtime is named on this command line.
    """
    for arg in cmd:
        if arg.startswith(("-lgnat", "-lgnarl")):
            return True

        if os.path.basename(arg).startswith(("libgnat", "libgnarl")):
            return True

    return False


def yes_no(value):
    return "yes" if value else "no"


def report(label, project, xvars, link_kind):
    print("== {} ==".format(label))

    for scope, extra_args in NOSTDLIB_SCOPES.items():
        jobs = build(project, extra_args, xvars)

        bind = jobs_of(jobs, "[Ada Bind]")
        link = jobs_of(jobs, link_kind)

        bind_cmd = command_of(bind[0]) if bind else []
        link_cmd = command_of(link[0]) if link else []

        print(
            "  {:8} - bind job: {}, bind -nostdlib: {}, "
            "{} job: {}, {} -nostdlib: {}, runtime linked: {}".format(
                scope,
                yes_no(bind),
                yes_no(has_nostdlib(bind_cmd)),
                link_kind,
                yes_no(link),
                link_kind,
                yes_no(has_nostdlib(link_cmd)),
                yes_no(bool(link) and links_runtime(link_cmd)),
            )
        )


#  A main always has a real bind phase, and the runtime is expected to be
#  needed (Ada.Text_IO is used).

report("main", "main.gpr", None, "[Link]")

#  Libraries: standalone ones get their own bind phase, like a main.
#  Non-standalone ones don't, and decide whether to still add the
#  runtime manually. Static libraries never reach either the -largs
#  forwarding or the binder-options forwarding, so nostdlib is expected to
#  have no observable effect on them at all.

for standalone in ("no", "yes"):
    for kind in ("static", "relocatable"):
        link_kind = "[Archive]" if kind == "static" else "[Link]"
        report(
            "lib standalone={} kind={}".format(standalone, kind),
            "lib.gpr",
            {"STANDALONE": standalone, "LIBRARY_KIND": kind},
            link_kind,
        )
