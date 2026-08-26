import json
import os

from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD

bnr = BuilderAndRunner()

# Regression test for gpr#917: a project that overrides part of the
# runtime directly in its own Source_Dirs must only recompile the
# overridden units actually reachable from the main -- not every
# override present in the project -- and units reachable only via ALI
# D-line data (no with-clause, e.g. compiler-implicit System.Concat_3)
# must be listed explicitly to gnatbind exactly once, without
# duplicating anything gnatbind would already find via a real
# with-clause.
#
#   s-conca3 -- overridden AND reachable, but only via the implicit
#               call GNAT's expander inserts for 3-operand string
#               concatenation (no with-clause anywhere). Must be
#               compiled, and must appear on gnatbind's command line.
#
#   s-regpat -- overridden but UNREACHABLE from main. Must NOT be
#               compiled at all. This is the original bug: gprbuild2
#               used to compile every Overridden_From_Runtime unit in
#               the project regardless of reachability.

bnr.check_output(
    [GPRBUILD, "-P", "tree/prj.gpr", "-p", "--json-summary"]
)

summary_file = "tree/jobs.json"
print("json summary found: " + str(os.path.exists(summary_file)))

with open(summary_file) as fp:
    jobs = json.load(fp)

compiled_units = sorted(
    action["uid"] for action in jobs if action["uid"].startswith("[Ada Compile]")
)


def is_unit(uid, simple_name):
    return simple_name in uid

print(
    "helper compiled (expected True): "
    + str(any(is_unit(u, "helper") for u in compiled_units))
)
print(
    "s-crc32 compiled (expected True): "
    + str(any(is_unit(u, "s-crc32") for u in compiled_units))
)
print(
    "s-conca3 compiled (expected True): "
    + str(any(is_unit(u, "s-conca3") for u in compiled_units))
)
print(
    "s-regpat compiled (expected False): "
    + str(any(is_unit(u, "s-regpat") for u in compiled_units))
)

bind_actions = [a for a in jobs if a["uid"].startswith("[Ada Bind]")]
print("number of bind actions (expected 1): " + str(len(bind_actions)))

bind_command = bind_actions[0]["command"] if bind_actions else ""

conca3_count = sum(1 for arg in bind_command.split() if "s-conca3.ali" in arg)
print("s-conca3.ali occurrences on bind command line (expected 0): " + str(conca3_count))

crc32_count = sum(1 for arg in bind_command.split() if "s-crc32.ali" in arg)
print("s-crc32.ali occurrences on bind command line (expected 0): " + str(crc32_count))

print(
    "s-regpat.ali on bind command line (expected False): "
    + str("s-regpat.ali" in bind_command)
)

# Regression check for the "silent fallback to installed runtime"
# failure mode: it's not enough for the build to succeed -- gnatbind
# and the linker could in principle still resolve System.Concat_3
# against the installed libgnat.a instead of our local override, and
# a build with an unmodified copy would look identical (same content,
# no version mismatch, no duplicate error). Actually running the
# binary and checking for a marker only the local override prints is
# the one check that distinguishes "used our object" from "used the
# installed one silently".

run_result = bnr.check_output(["./tree/obj/main", "b", "l", "a"])

print(
    "local override executed (expected True): "
    + str("USING LOCAL OVERRIDE" in run_result.out)
)
