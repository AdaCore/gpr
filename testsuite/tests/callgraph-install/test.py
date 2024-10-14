import os
import subprocess

from testsuite_support.builder_and_runner import BuilderAndRunner, GPRINSTALL, GPRBUILD

bnr = BuilderAndRunner()

def run(args):
    bnr.check_output(args)


def check_exists(filename, invert=False):
    if_tag, else_tag = "OK", "NOK"
    if invert:
        if_tag, else_tag = else_tag, if_tag
    print("{} {}".format(
        if_tag if os.path.exists(filename) else else_tag,
        os.path.basename(filename)
    ))


inst_prefix = "--prefix=" + os.getcwd() + "/inst"
instl_prefix = "--prefix=" + os.getcwd() + "/instl"

# Test a standard project
bnr.check_output([GPRBUILD, "-p", "-q", "prj.gpr"])
run([GPRINSTALL, "-p", inst_prefix, "prj.gpr"])
check_exists("inst/lib/prj/pck.ci")
check_exists('inst/lib/prj/pck2.ci')

run([GPRINSTALL, "-p", "--uninstall", inst_prefix, "prj.gpr"])
check_exists('inst/lib/prj/pck.ci', invert=True)
check_exists('inst/lib/prj/pck2.ci', invert=True)

# Test a library project
bnr.check_output([GPRBUILD, "-p", "-q", "lib.gpr"])
run([GPRINSTALL, "-p", instl_prefix, "lib.gpr"])
check_exists('instl/lib/lib/pck.ci')
check_exists('instl/lib/lib/pck2.ci')

run([GPRINSTALL, "-p", "--uninstall", instl_prefix, "lib.gpr"])
check_exists('instl/lib/lib/pck.ci', invert=True)
check_exists('instl/lib/lib/pck2.ci', invert=True)
