import os
import os.path
from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD

bnr = BuilderAndRunner()

bnr.build("prj.gpr")
current_path = os.environ['PATH']
os.environ['PATH'] = f"{os.path.join(os.getcwd(), 'tree/bin')}:{current_path}"
out = bnr.run(["obj/main"])
for line in out.out.split("\n"):
	print(line)
