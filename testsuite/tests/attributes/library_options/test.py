from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRCLEAN, GPRBUILD
import json
import re
import os

g3_option_pattern = r"Link\].*\(lib_with_library_options\.gpr\)"


def grep_file_for_g3(path):
    """Search for the '-g3' option in link commands within a JSON build log file.

    Opens a JSON file containing build commands and looks for entries with UIDs
    matching the link pattern for lib_with_library_options.gpr, then checks if
    the '-g3' option is present in the command.
    """
    try:
        with open(path, "r") as f:
            data = json.load(f)
            for entry in data:
                uid = entry.get("uid", "")
                if re.search(g3_option_pattern, uid):
                    return "-g3" in entry["command"]
        return False
    except FileNotFoundError:
        print("File not found:", path)
        return False


def run_test(
    bnr,
    project_file,
    env,
    test_name,
    run_with_coverage_and_valgrind_wrapper,
    check_g3_path=None,
):
    """Run a single test case with gprbuild and optional -g3 check."""
    print(f"=== {test_name} ===")
    if run_with_coverage_and_valgrind_wrapper:
        p = bnr.run(
            [GPRBUILD, f"-P{project_file}", "-p", "-q", "--json-summary"],
            env=env,
        )
    else:
        p = bnr.simple_run(
            [GPRBUILD, f"-P{project_file}", "-p", "-q", "--json-summary"], env=env
        )
    if p.status == 0:
        print("OK")
    else:
        print(p.out)
    if p.status == 0 and check_g3_path:
        if grep_file_for_g3(check_g3_path):
            print("Found '-g3' in the link command")
        else:
            print("ERROR: did not find '-g3' in the link command")
    if run_with_coverage_and_valgrind_wrapper:
        bnr.run([GPRCLEAN, f"-P{project_file}", "-r"], env=env)
    else:
        bnr.simple_run([GPRCLEAN, f"-P{project_file}", "-r"], env=env)
    print("")


bnr = BuilderAndRunner()

# Compile foo.o which is imported by the libs
bnr.simple_run([GPRBUILD, "-q", "-Ptree/foo/foo.gpr"])

library_kinds = ["relocatable_standalone", "relocatable", "static_standalone", "static"]

# Test libraries importing other libraries that contain library_options

run_with_coverage_and_valgrind_wrapper = True

for kind_with_library_options in library_kinds:
    for importing_library_kind in library_kinds:
        if importing_library_kind in [
            "relocatable",
            "relocatable_standalone",
        ] and kind_with_library_options in ["static", "static_standalone"]:
            # A relocatable library cannot import a static library
            continue

        env = {
            "GPR_PROJECT_PATH": os.path.join(
                os.getcwd(),
                "tree",
                "lib-with-library-options",
                kind_with_library_options,
            )
        }

        project_file = f"tree/importing-lib/{importing_library_kind}/importing_lib.gpr"
        test_name = (
            f"[LIB] {importing_library_kind} importing {kind_with_library_options}"
        )
        if kind_with_library_options == "static":
            check_g3_path = None  # Only object files are accepted for static libs in Library_Options, so we do not check for the -g3 option
        else:
            check_g3_path = f"tree/importing-lib/{importing_library_kind}/jobs.json"

        run_test(
            bnr,
            project_file,
            env,
            test_name,
            run_with_coverage_and_valgrind_wrapper,
            check_g3_path,
        )
        run_with_coverage_and_valgrind_wrapper = False

        if kind_with_library_options == "static":
            # Also test the faulty variant which adds -g3 to library options
            faulty_project_file = (
                f"tree/importing-lib/{importing_library_kind}/faulty_importing_lib.gpr"
            )
            faulty_test_name = f"[LIB] faulty {importing_library_kind} importing {kind_with_library_options}"
            run_test(
                bnr, faulty_project_file, env, faulty_test_name, True, check_g3_path
            )


run_with_coverage_and_valgrind_wrapper = True

# Finally test the main program importing a library with library_options
for kind_with_library_options in library_kinds:
    env = {
        "GPR_PROJECT_PATH": os.path.join(
            os.getcwd(), "tree", "lib-with-library-options", kind_with_library_options
        )
    }
    project_file = "tree/importing-main/prj.gpr"
    test_name = f"[MAIN] importing {kind_with_library_options}"

    if kind_with_library_options == "static":
        check_g3_path = None  # Only object files are accepted for static libs in Library_Options, so we do not check for the -g3 option
    else:
        check_g3_path = f"tree/importing-main/jobs.json"

    run_test(
        bnr,
        project_file,
        env,
        test_name,
        run_with_coverage_and_valgrind_wrapper,
        check_g3_path,
    )
    run_with_coverage_and_valgrind_wrapper = False

    if kind_with_library_options == "static":
        # Also test the faulty variant which adds -g3 to library options
        # despite only object files can be specified.
        faulty_project_file = "tree/importing-main/faulty_prj.gpr"
        faulty_test_name = f"[MAIN] Faulty importing {kind_with_library_options}"
        run_test(bnr, faulty_project_file, env, faulty_test_name, True)
