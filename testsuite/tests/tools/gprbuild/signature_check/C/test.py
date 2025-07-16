from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD
import json
import os
import re
bnr = BuilderAndRunner()

def edit(file, before, after):
	old = ""
	with open (file, "r") as f:
		old = f.read()

	with open (file, "w") as f:
		for line in old.split("\n"):
			if before in line:
				new_line = re.sub(before, after, line)
				f.write(f"{new_line}\n")
			else:
				f.write(f"{line}\n")

def run(cmd):
	print("$ " + " ".join(cmd));
	if cmd[0] == GPRBUILD:
		bnr.call(cmd)
	else:
		print(bnr.simple_run([cmd], catch_error=True).out)

def test():
	run([GPRBUILD, "-Pprj", "-p", "--json-summary", "-q", "-j1"])
	with open(os.path.join("jobs.json")) as fp:
		cnt = json.load(fp)

	uids = dict((job["uid"], job["status"]) for job in cnt)
	for uid in sorted(uids.keys()):
		print(f"{uid}: {uids[uid]}")
	run(["./obj/main"])

# Initial build
test()

# Check no rebuild
test()

# Change main.c, should retrigger compile main.c / link main
edit("src/main.c", "from main", "from foo")
test()

# Change include.c, should retrigger compile include.c / archive / link main
edit("src/include.c", "from include", "from bar")
test()

# Change constant.h, should retrigger compile main.c include.c / archive / link main
edit("src/constant.h", "1", "2")
test()
