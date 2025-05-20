import os
from e3.env import Env
from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPR2BUILD

bnr = BuilderAndRunner()

if 'windows' in Env().target.platform:
    exeext = '.exe'
else:
    exeext = ''


def run(cmd):
    print("$ " + " ".join(cmd))
    if cmd[0] == GPR2BUILD:
        bnr.call(cmd)
    else:
        print(bnr.simple_run([cmd], catch_error=True).out)


# Basic check that building demo.gpr produces libdemo.a and that building an
# exe with it only uses libdemo.a and not any of the objects contained in it.

for dir in ("library_interface", "interfaces"):
    for app_dir in ("app", "app2"):
        for library_symbol_policy in ("restricted", "unrestricted"):
            print("========================")
            print(f"lib attribute: {dir}")
            print(f"app sources: {app_dir}")
            print(f"symbol policy: {library_symbol_policy}")
            run(
                [
                    GPR2BUILD,
                    f"-P{os.path.join(dir, "app.gpr")}",
                    f"-XSRC_DIR={app_dir}",
                    f"-XSYM_POLICY={library_symbol_policy}",
                    "-f",
                    "-q",
            ])
            if os.path.exists("main" + exeext):
                run(["./main"])
                os.unlink("main" + exeext)
