import subprocess

from testsuite_support.builder_and_runner import BuilderAndRunner

subprocess.check_output("gprbuild -p -q files/p.gpr", shell=True)
BuilderAndRunner().build("p.gpr", args=['-p'])
BuilderAndRunner().call(["./main"])
