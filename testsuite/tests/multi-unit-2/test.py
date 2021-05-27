import subprocess

from testsuite_support.builder_and_runner import BuilderAndRunner

subprocess.check_call("gprbuild -p -q files/p.gpr", shell=True)
BuilderAndRunner().build("p.gpr", args=['-p'])
subprocess.check_call(["./main"])
subprocess.check_call("gprbuild -p -q files/p.gpr", shell=True)
subprocess.check_call(["gprls", "-Pfiles/p.gpr", "few_units~2.ali"])
subprocess.check_call(["gprls", "-Pfiles/p.gpr", "few_units~4.ali"])
subprocess.check_call(["gprls", "-Pfiles/p.gpr", "few_units~6.ali"])
subprocess.check_call(["gprls", "-Pfiles/p.gpr", "few_units~7.ali"])
