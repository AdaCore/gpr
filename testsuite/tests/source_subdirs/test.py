from testsuite_support.builder_and_runner import BuilderAndRunner, GPRLS


def execute(cmd):
    print("------------------")
    print(" ".join(cmd))
    print("")
    bnr.call(cmd)


bnr = BuilderAndRunner()

for prj in "prj1", "prj2":
    bnr.call(["gprbuild", f"-P{prj}", "--src-subdirs=src2", "-q"])
    execute([GPRLS, f"-P{prj}", "-s", "--src-subdirs=src2"])
