from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD, GPRCLEAN, GPRINSTALL, GPRLS, GPRINSPECT


bnr = BuilderAndRunner()

all_tools = [GPRBUILD, GPRCLEAN, GPRINSTALL, GPRINSPECT]


def execute(cmd, check=False):
    print("-------------------------")
    print(" ".join(cmd))
    print("-------------------------")
    if check:
        bnr.check_call(cmd)
    else:
        p = bnr.call(cmd)
        if p.status != 0:
            print(f"STATUS: {p.status}")
    print("")


# check gprtool -h

for t in all_tools:
    execute([t, "-h"], True)

# check gprtool --help works the same

for t in all_tools:
    execute([t, "--help"], True)

# check invalid option

for t in all_tools:
    execute([t, "--no-such-switch"])

# check invalid parameter delimiter

for t in all_tools:
    execute([t, "--config:conf.cgpr"])

# check no parameter

for t in all_tools:
    execute([t, "-P"])

# check implicit project

execute([GPRBUILD, "-j1"], True)

# test -P <missing_project> -q

execute([GPRBUILD, "-P", "-q"])

# check project without -P, and non-positional + positional arguments

execute([GPRCLEAN, "demo.gpr", "pkg.ads", "-n"], True)

# check several projects defined

execute([GPRCLEAN, "-P", "demo.gpr", "demo2.gpr"])
execute([GPRCLEAN, "demo.gpr", "demo2.gpr"])

# check optional space

execute([GPRCLEAN, "-Pdemo", "-n"], True)

# check index

execute([GPRCLEAN, "--RTS:ada", "default", "-n", "demo.gpr"])

# check index + equal delimiter

execute([GPRCLEAN, "--RTS:ada=default", "-n", "demo.gpr"])

# check optional index

execute([GPRCLEAN, "--RTS", "default", "-n", "demo.gpr"])
execute([GPRCLEAN, "--RTS=default", "-n", "demo.gpr"])
