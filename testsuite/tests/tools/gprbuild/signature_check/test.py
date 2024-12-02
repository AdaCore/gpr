from testsuite_support.builder_and_runner import BuilderAndRunner
import json
import os
bnr = BuilderAndRunner()

def run(cmd):
    print("$ " + " ".join(cmd));
    if cmd[0] == "gpr2build":
        bnr.call(cmd)
    else:
        print(bnr.simple_run([cmd], catch_error=True).out)

def test(variant):
    run(["gpr2build", "-Pprj", "-p", f"-XVARIANT={variant}", "--json-summary", "-q"])
    with open(os.path.join("jobs.json")) as fp:
        cnt = json.load(fp)

    uids = dict((job["uid"], job["status"]) for job in cnt)
    for uid in sorted(uids.keys()):
        print(f"{uid}: {uids[uid]}")
    run(["./main"])

# check initial build
test("1")

# check rebuild with same variant: all must be skipped
test("1")

# check build with another variant, all must be rebuilt, new output for main
test("2")

# check rebuild with same variant: all must be skipped
test("2")

# check build with old variant: all must be rebuilt except pkg1.o
test("1")
