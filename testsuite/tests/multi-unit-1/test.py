import subprocess

from testsuite_support.builder_and_runner import BuilderAndRunner

subprocess.check_output("gprbuild -p -q files/multi.gpr", shell=True)
BuilderAndRunner().build("multi.gpr", args=['-p'])
BuilderAndRunner().call(["./main"])
