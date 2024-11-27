import os
import os.path
from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD

bnr = BuilderAndRunner()

current_path = os.environ['PATH']

bindir = os.path.join(os.getcwd(), 'tree/bin')
os.environ['PATH'] = f"{bindir}:{current_path}"
bnr.simple_run([GPRBUILD, "stub.gpr"])
bnr.simple_run([GPRBUILD, "-XPREFIX=bla-", "stub.gpr"])

bnr.build("prj.gpr")
out = bnr.run(["obj/main"])
for line in out.out.split("\n"):
	print(line)
