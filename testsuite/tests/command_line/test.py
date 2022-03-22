from testsuite_support.builder_and_runner import (
    BuilderAndRunner,
    GPRLS,
    GPRCLEAN,
    GPRINSTALL,
    GPRNAME,
)

bnr = BuilderAndRunner()

all_tools = [GPRLS, GPRCLEAN, GPRINSTALL, GPRNAME]


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
    if t == GPRNAME:
        execute([t, "-gnatep:foo"])
    else:
        execute([t, "--config:conf.cgpr"])

# check no parameter

for t in all_tools:
    execute([t, "-P"])

# check implicit project

execute([GPRLS, "-s"], True)

# check project without -P, and non-positional + positional arguments

execute([GPRLS, "demo.gpr", "pkg.ads", "-s"], True)

# check several projects defined

execute([GPRLS, "-P", "demo.gpr", "demo2.gpr"])
execute([GPRLS, "demo.gpr", "demo2.gpr"])

# check optional space

execute([GPRLS, "-Pdemo", "-s"], True)

# check index

execute([GPRLS, "--RTS:ada", "default", "-s", "demo.gpr"])

# check index + equal delimiter

execute([GPRLS, "--RTS:ada=default", "-s", "demo.gpr"])

# check optional index

execute([GPRLS, "--RTS", "default", "-s", "demo.gpr"])
execute([GPRLS, "--RTS=default", "-s", "demo.gpr"])
