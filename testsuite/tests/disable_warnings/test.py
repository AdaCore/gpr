from testsuite_support.builder_and_runner import (
    BuilderAndRunner,
    GPRLS,
    GPRCLEAN,
)

bnr = BuilderAndRunner()

all_tools = [GPRLS, GPRCLEAN]


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


# check gprtool

for t in all_tools:
    execute([t, "-Pdemo"], True)

# check gprtool -ws

for t in all_tools:
    execute([t, "-Pdemo", "-ws"], True)
