from testsuite_support.builder_and_runner import BuilderAndRunner, GPRLS
import subprocess


def build():
    subprocess.check_call("gprbuild -p -q files/p.gpr", shell=True)


bnr = BuilderAndRunner()

build()
bnr.build("p.gpr", args=["-p"])
bnr.check_call(["./main"])
build()
bnr.check_call([GPRLS, "-Pfiles/p.gpr", "few_units~2.ali"])
bnr.check_call([GPRLS, "-Pfiles/p.gpr", "few_units~4.ali"])
bnr.check_call([GPRLS, "-Pfiles/p.gpr", "few_units~6.ali"])
bnr.check_call([GPRLS, "-Pfiles/p.gpr", "few_units~7.ali"])
bnr.check_call([GPRLS, "-Pfiles/p.gpr", "pkg.ali"])
