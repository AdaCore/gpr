import os
from e3.env import Env
from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD

bnr = BuilderAndRunner()
env = Env()

if 'windows' in env.target.platform:
    exeext = '.exe'
else:
    exeext = ''


def run(cmd):
    print("$ " + " ".join(cmd))
    if cmd[0] == GPRBUILD:
        return bnr.call(cmd)
    else:
        print(bnr.simple_run([cmd], catch_error=True).out)

# Basic check that building demo.gpr produces libdemo.a and that building an
# exe with it only uses libdemo.a and not any of the objects contained in it.

# as we produce shared libs, we need to add the path for windows to retrieve
# them (no rpath mechanism for dlls)

env.add_search_path("PATH", "lib")

for dir in ("library_interface", "interfaces"):
    for app_dir in ("app", "app2"):
        for library_symbol_policy in ("restricted", "unrestricted"):
            print("========================")
            print(f"lib attribute: {dir}")
            print(f"app sources: {app_dir}")
            print(f"symbol policy: {library_symbol_policy}")
            run(
                [
                    GPRBUILD,
                    f"-P{os.path.join(dir, "app.gpr")}",
                    f"-XSRC_DIR={app_dir}",
                    f"-XSYM_POLICY={library_symbol_policy}",
                    "-f",
                    "-q",
            ])
            if os.path.exists("main" + exeext):
                run(["./main"])
                os.unlink("main" + exeext)
