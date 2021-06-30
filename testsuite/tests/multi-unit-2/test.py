from testsuite_support.builder_and_runner import BuilderAndRunner, GPRLS
import subprocess


def build():
    subprocess.check_call("gprbuild -p -q files/p.gpr", shell=True)


build()
BuilderAndRunner().build("p.gpr", args=['-p'])
subprocess.check_call(["./main"])
build()
subprocess.check_call([GPRLS, "-Pfiles/p.gpr", "few_units~2.ali"])
subprocess.check_call([GPRLS, "-Pfiles/p.gpr", "few_units~4.ali"])
subprocess.check_call([GPRLS, "-Pfiles/p.gpr", "few_units~6.ali"])
subprocess.check_call([GPRLS, "-Pfiles/p.gpr", "few_units~7.ali"])
subprocess.check_call([GPRLS, "-Pfiles/p.gpr", "pkg.ali"])
