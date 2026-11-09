"""Build fixed projects through a compiler wrapper that detects overlap."""

import os
from pathlib import Path
import re
import shutil

from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD


bnr = BuilderAndRunner()
switch = "--single-compile-per-obj-dir"

# Keep the ordinary cases independent of any jobserver inherited from the
# testsuite's launcher. The last case creates its own make jobserver.
os.environ.pop("MAKEFLAGS", None)

# Build the wrapper before selecting it as the project compiler driver.
# It does not use libgpr2, so build it without coverage instrumentation.
# It forwards compiler arguments to gcc, detects overlapping invocations in
# each working directory, and logs their lifetimes for the checks below.
bnr.simple_run([GPRBUILD, "-P", "wrapper.gpr", "-p", "-q"])
wrapper = Path("wrapper/compiler_wrapper" + (".exe" if os.name == "nt" else ""))
os.environ["SINGLE_COMPILE_WRAPPER"] = wrapper.resolve().as_posix()
os.environ["SINGLE_COMPILE_CC"] = shutil.which("gcc")


def find_make():
    # A native Windows gprbuild cannot use Cygwin/MSYS jobserver descriptors.
    # Search past those installations when they precede native make in PATH.
    checked = {}
    for directory in os.get_exec_path():
        make = shutil.which("make", path=directory or os.curdir)
        if make is None or make in checked:
            continue
        version = bnr.simple_run([make, "--version"]).out
        checked[make] = version
        match = re.search(r"GNU Make (\d+)\.(\d+)", version)
        if not match or tuple(map(int, match.groups())) < (4, 2):
            continue
        if os.name == "nt" and not re.search(
            r"^Built for .*?(mingw|windows|win32)", version, re.I | re.M
        ):
            continue
        return make, version, tuple(map(int, match.groups()))
    raise AssertionError(
        "No compatible GNU Make >= 4.2 found in PATH"
        + (" (native Windows build required)" if os.name == "nt" else "")
        + "\n" + "\n".join(f"{path}:\n{version}"
                           for path, version in checked.items())
    )


def build(label, project, expected, *, serial=True, parallel=1, fail=False,
          args=(), option=True, ada=False, jobserver=False):
    # Give each case fresh logs and a fresh marker for the injected failure.
    # Keep object files between cases so incremental and forced builds can be
    # tested against the outputs of earlier builds.
    events = Path("events", label).resolve()
    events.mkdir(parents=True)
    # serial enables overlap detection: the wrapper fails if two compilations
    # use the same object directory; it does not serialize them itself.
    # parallel asks wrappers to wait until that many compilations have started,
    # making a lack of concurrency visible. fail makes the first attempt fail.
    os.environ.update({
        "SINGLE_COMPILE_EVENTS": str(events),
        "SINGLE_COMPILE_SERIAL": "1" if serial else "0",
        "SINGLE_COMPILE_PARALLEL": str(parallel),
        "SINGLE_COMPILE_FAIL_FIRST": "1" if fail else "0",
    })

    # Request four jobs even when the option should serialize a directory.
    # option controls only the command-line switch: a project can also enable
    # it through Builder.Switches, as one of the cases below does.
    command = [GPRBUILD, "-P", f"tree/{project}.gpr", "-p", "-q", "-j4"]
    if option:
        command.append(switch)
    if not ada:
        # The C projects have no mains. Compile all sources, including those
        # in imported projects. Ada mains instead follow the normal build flow
        # so that binding and compilation of binder-generated sources run too.
        command.append("-U")
    command.extend(args)

    if jobserver:
        # The makefile invokes gprbuild with the tested option, -j4, and -f.
        # Its recursive recipe passes make's two-job budget to gprbuild.
        make, version, number = find_make()
        command = [make, "-f", "jobserver.mk", "-j2", "GPRBUILD=" + GPRBUILD]
        if number >= (4, 4):
            # Select the protocol supported by native gprbuild explicitly.
            # Older compatible make versions already use these defaults.
            command.append("--jobserver-style="
                           + ("sem" if os.name == "nt" else "pipe"))

    # Only the injected-failure case should fail. Check wrapper diagnostics as
    # well: an expected build failure must not hide a concurrency violation.
    result = bnr.run(command)
    details = (f"make: {make}\n{version}\n{result.out}"
               if jobserver else result.out)
    assert (result.status != 0) == fail, details
    assert "simultaneous compilations" not in result.out, details
    assert "did not run in parallel" not in result.out, details
    if jobserver:
        auth = re.search(r"^MAKEFLAGS=.*--jobserver-auth=(\S+)",
                         result.out, re.M)
        assert auth, details
        if os.name == "nt":
            assert not auth[1].startswith("fifo:"), details
            assert not re.fullmatch(r"-?\d+,-?\d+", auth[1]), details

    # Each attempted compilation produces one log with its wrapper's start and
    # finish timestamps, including an attempt deliberately failed before gcc.
    # Check the attempt count to catch missing, deferred, or redundant work.
    logs = list(events.glob("*.log"))
    assert len(logs) == expected, (label, logs, details)
    changes = []
    for log in logs:
        start, finish = map(float, log.read_text().splitlines())
        changes.extend(((start, 1), (finish, -1)))
    # Sweep the intervals to count simultaneously active wrappers. A finish
    # sorts before a start at the same timestamp, so touching intervals do not
    # count as overlap. The peak verifies the requested concurrency.
    active = peak = 0
    for _, change in sorted(changes):
        active += change
        peak = max(peak, active)
    if expected:
        assert peak >= parallel, (label, peak, details)
    if jobserver:
        # gprbuild requested four jobs, but make granted a budget of two.
        assert peak <= 2, (label, peak, details)
    if ada:
        # Both mains must reach the compilation of their binder-generated
        # source; those compilations need the same per-directory protection.
        assert sum(log.name.startswith("b__") for log in logs) == 2
    print(label + ": OK")


# Verify that the new command-line option is discoverable in the help output.
assert switch in bnr.simple_run([GPRBUILD, "--help"]).out

# Three C sources share one object directory. With four jobs available, the
# option must still prevent the wrapper from observing any overlap there.
build("single directory", "single", 3)

# Reuse those objects unchanged: an up-to-date build must invoke no compiler
# and must finish even though every compilation action is skipped.
build("incremental build", "single", 0)

# Force the same three sources to compile again, still with no overlap in
# their object directory. This also prepares existing objects for later cases.
build("forced rebuild", "single", 3, args=("-f",))

# Projects A and B each have three sources and their own object directory.
# Require at least two overlapping wrappers across directories while keeping
# per-directory overlap checks enabled: the option must preserve parallelism.
build("parallel directories", "parallel", 6, parallel=2)

# Move A and B into one shared object directory. B spells it using '.' and
# '..', so all six compilations must serialize by normalized directory path,
# rather than by project identity or the literal Object_Dir spelling.
build("shared directory", "parallel", 6, args=("-XOBJECT_LAYOUT=shared",))

# Omit the command-line option. Builder.Switches ("C") in builder.gpr must
# enable the same serialization for its three sources.
build("Builder switch", "builder", 3, option=False)

# Force all three sources to run and fail the first attempt. With -k, the
# scheduler must release its directory reservation and retry the other ready
# compilations that were deferred while that directory was busy. All three
# attempts must be logged, although the overall build is expected to fail.
build("keep going after failure", "single", 3, fail=True, args=("-f", "-k"))

# Build two Ada mains through binding and linking. Expect four compilations:
# the two mains and their two binder-generated sources. All share one object
# directory, so ordinary and post-bind compilations must both serialize.
build("Ada and binder compilations", "ada_mains", 4, ada=True)

# Force the single-directory project again with the option absent. Disable
# the wrapper's per-directory rejection and require overlap, checking that
# normal parallel compilation in one directory remains available by default.
build("default parallelism", "single", 3,
      option=False, serial=False, parallel=2, args=("-f",))

# Use three independent object directories under make -j2, while gprbuild
# requests -j4. Force all nine sources to compile, including A and B built
# earlier. Three directories provide enough independent work to expose a
# scheduler exceeding make's budget: the measured peak must be exactly two,
# with no overlap inside any individual directory.
build("jobserver", "jobserver", 9, parallel=2, jobserver=True)
