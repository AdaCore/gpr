from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD, GPRCLEAN

from pathlib import Path

bnr = BuilderAndRunner()

def run(cmd):
    print("$ " + " ".join(cmd))
    if cmd[0] in (GPRBUILD, GPRCLEAN):
        bnr.call(cmd)
    else:
        print(bnr.simple_run([cmd], catch_error=True).out)

def delete_files(path):
    print(f"$ rm -f {path}/*")
    for file in path.iterdir():
        if file.is_file():
            file.unlink()

lib_path = Path("lib/lib")

run([GPRBUILD, "-P", "main.gpr", "-XKIND=va"])
run(["./main"])
delete_files(lib_path)
run([GPRBUILD, "-P", "main.gpr", "-XKIND=vb"])
run(["./main"])
delete_files(lib_path)
run([GPRBUILD, "-P", "main.gpr", "-XKIND=va"])
run(["./main"])
delete_files(lib_path)
run([GPRBUILD, "-P", "main.gpr", "-XKIND=vb"])
run(["./main"])
