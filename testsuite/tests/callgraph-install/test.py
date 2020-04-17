import os
import subprocess


def run(args_pattern):
    subprocess.check_output(args_pattern.format(os.getcwd()), shell=True)


def check_exists(filename, invert=False):
    if_tag, else_tag = "OK", "NOK"
    if invert:
        if_tag, else_tag = else_tag, if_tag
    print("{} {}".format(
        if_tag if os.path.exists(filename) else else_tag,
        os.path.basename(filename)
    ))


# Test a standard project
run("gprbuild -p -q prj.gpr")
run("gpr2install -p  --prefix={}/inst prj.gpr")
check_exists("inst/lib/prj/pck.ci")
check_exists('inst/lib/prj/pck2.ci')

run("gpr2install -p  --uninstall --prefix={}/inst prj.gpr")
check_exists('inst/lib/prj/pck.ci', invert=True)
check_exists('inst/lib/prj/pck2.ci', invert=True)

# Test a library project
run('gprbuild -p -q lib.gpr')
run('gpr2install -p  --prefix={}/instl lib.gpr')
check_exists('instl/lib/lib/pck.ci')
check_exists('instl/lib/lib/pck2.ci')

run('gpr2install -p  --uninstall --prefix={}/instl lib.gpr')
check_exists('instl/lib/lib/pck.ci', invert=True)
check_exists('instl/lib/lib/pck2.ci', invert=True)
