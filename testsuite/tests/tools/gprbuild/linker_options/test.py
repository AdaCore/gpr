from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD, GPRCLEAN

from pathlib import Path

bnr = BuilderAndRunner()

def run(cmd, output=""):
    print("$ " + " ".join(cmd))
    if cmd[0] in (GPRBUILD):
        if output:
            bnr.simple_run(cmd, catch_error=False, output=output)
        else:
            bnr.call(cmd)


def extract_stack_options(filepath):
    results = []
    in_block = False
    lines = open(filepath).readlines()

    for i, line in enumerate(lines):
        stripped = line.strip()

        if stripped == "--  BEGIN Object file/option list":
            in_block = True
            continue
        if stripped == "--  END Object file/option list":
            in_block = False
            continue

        if in_block:
            # Strip the Ada comment prefix "   --   " or "   -- "
            if stripped.startswith("--"):
                content = stripped.lstrip("-").strip()

                if content.startswith("-Xlinker"):
                    # Peek at the next non-empty comment line for the --stack value
                    for next_line in lines[i+1:]:
                        next_stripped = next_line.strip()
                        if next_stripped.startswith("--"):
                            next_content = next_stripped.lstrip("-").strip()
                            if next_content.startswith("--stack"):
                                results.append(f"-Xlinker {next_content}")
                            break

                elif content.startswith("-Wl,--stack"):
                    results.append(content)

    return results


def check_result(out, options):
    with open(out) as f:
        content = f.read()

    first = options[0]
    rest  = options[1:]

    if first not in content:
        print(f"FAIL: expected '{first}' not found in the link command line")
    else:
        print(f"OK: '{first}' found")

    for opt in rest:
        if opt in content:
            print(f"FAIL: '{opt}' should not be in the link command line")
        else:
            print(f"OK: '{opt}' absent")


logfile = "log.out"

run([GPRBUILD, "-q", "-P", "my_gnatbind/gnatbind.gpr"])
run([GPRBUILD, "-v", "-P", "tree/prj.gpr", "-bargs", f"--gnatbind_path={Path.cwd() / "my_gnatbind/main"}"], output=logfile)
check_result(logfile, extract_stack_options("tree/obj/b__main.adb"))





