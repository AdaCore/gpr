from testsuite_support.builder_and_runner import BuilderAndRunner, GPRLS, GPRCONFIG

CGPR = "config.cgpr"

bnr = BuilderAndRunner()


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


execute([GPRCONFIG, "--batch", "--config=Ada", "-o", CGPR], check=True)
execute([GPRLS, "--config", CGPR, "--target=aarch64-elf", "-s"])
execute([GPRLS, "--config", CGPR, "-XTARGET=aarch64-elf", "-s"])

target = None
with open(CGPR, "r") as fp:
    for line in fp.read().splitlines():
        if "for Target use" in line:
            _, target, _ = line.split('"')
            break

execute([GPRLS, "--config", CGPR, f"--target={target}", "-s"])
execute([GPRLS, "--config", CGPR, f"-XTARGET={target}", "-s"])
