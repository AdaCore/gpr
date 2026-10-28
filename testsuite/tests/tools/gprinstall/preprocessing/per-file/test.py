import os

from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD, GPRINSTALL

bnr = BuilderAndRunner()

PREFIX = "inst"

#  (installed filename, preprocessing symbol, substituted value)
#  symbol is None for a file with nothing to preprocess.

UNITS = [
    ("spec_only.ads", "DEF_A", "111"),
    ("pair_both_prep.ads", "DEF_B_SPEC", "222"),
    ("pair_both_prep.adb", "DEF_B_BODY", "333"),
    ("pair_body_only_prep.ads", None, None),
    ("pair_body_only_prep.adb", "DEF_C_BODY", "444"),
    ("pair_with_separate.ads", None, None),
    ("pair_with_separate.adb", None, None),
    ("pair_with_separate-show.adb", "DEF_SEP_VALUE", "555"),
]


def report(filename, symbol, value):
    path = os.path.join(PREFIX, "include", "prj", filename)

    try:
        with open(path) as fp:
            content = fp.read()
    except OSError as e:
        print("{}: cannot read installed file ({})".format(filename, e))
        return

    if symbol is None:
        print("{}: nothing to preprocess here".format(filename))
        return

    has_value = value in content
    has_symbol = ("$" + symbol) in content

    print(
        "{}: substituted value present: {}, raw $symbol present: {}".format(
            filename, has_value, has_symbol
        )
    )


print("$ gprbuild -p -q prj.gpr")
bnr.run([GPRBUILD, "-p", "-q", "prj.gpr"])

print(
    "$ gprinstall --prefix="
    + PREFIX
    + " -f -p --sources-only prj.gpr"
)
bnr.run(
    [GPRINSTALL, "--prefix=" + PREFIX, "-f", "-p", "--sources-only", "prj.gpr"]
)

for filename, symbol, value in UNITS:
    report(filename, symbol, value)
