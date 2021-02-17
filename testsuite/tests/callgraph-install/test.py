import os
import subprocess

from testsuite_support.builder_and_runner import BuilderAndRunner, GPRINSTALL


def run(args):
    BuilderAndRunner().check_output(args)


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
subprocess.check_output("gprbuild -p -q prj.gpr", shell=True)
run([GPRINSTALL, "-p", inst_prefix, "prj.gpr"])
check_exists("inst/lib/prj/pck.ci")
check_exists('inst/lib/prj/pck2.ci')

run([GPRINSTALL, "-p", "--uninstall", inst_prefix, "prj.gpr"])
check_exists('inst/lib/prj/pck.ci', invert=True)
check_exists('inst/lib/prj/pck2.ci', invert=True)

# Test a library project
subprocess.check_output("gprbuild -p -q lib.gpr", shell=True)
run([GPRINSTALL, "-p", instl_prefix, "lib.gpr"])
check_exists('instl/lib/lib/pck.ci')
check_exists('instl/lib/lib/pck2.ci')

run([GPRINSTALL, "-p", "--uninstall", instl_prefix, "lib.gpr"])
check_exists('instl/lib/lib/pck.ci', invert=True)
check_exists('instl/lib/lib/pck2.ci', invert=True)
