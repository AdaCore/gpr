from testsuite_support.builder_and_runner import BuilderAndRunner
from e3.env import Env
import os
import os.path

def run(makeflags, set=True):
	if set:
		print(f'Testcase - Makeflags = "{makeflags}"')
		os.environ['MAKEFLAGS'] = makeflags
	else:
		print("Testcase - Makeflags = UNSET")
		del os.environ['MAKEFLAGS']

	out = bnr.run(["obj/main"])
	for line in out.out.split("\n"):
		print(line)

bnr = BuilderAndRunner()
bnr.build("prj.gpr")

# Simple Pipe

if Env().host.platform.endswith('windows') or Env().host.platform.endswith('windows64'):
	pass # Fstat on windows fails the test, but this is already functionally validated 
else:
	run("--jobserver-auth=1,2")
	run("--jobserver-auth=1,2 ")
run("--jobserver-auth=-1,2")  # Invalid FD
run("--jobserver-auth=1,-2")  # Invalid FD
run("--jobserver-auth=-1,-2") # Invalid FD

# Named Pipe
run("--jobserver-auth=fifo:prj.gpr")
run("--jobserver-auth=fifo:prj.gpr ")
run("--jobserver-auth=fifo:unknown")  # File doesn't exist

# Semaphore
run("--jobserver-auth=bla")
run("--jobserver-auth=bla ")

# Dry-run
run("n --jobserver-auth=1,2")

# No makeflags
run("", False)

# Empty makeflags
run("")

# Emulate make < 4.2 by not using --jobserver-auth
run("--foo-bar=1,2")

# Make sure we have the right kind of jobserver if multiple declaration (latest --jobserver-auth)
run("--jobserver-auth=1,2 --jobserver-auth=fifo:prj.gpr") # Should be a Named Pipe
run("--jobserver-auth=1,2 --jobserver-auth=prj.gpr")      # Should be a Named Semaphore (or nothing on UNIX)
