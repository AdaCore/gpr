import os
import re
from testsuite_support.builder_and_runner import BuilderAndRunner, GPRCLEAN


def check_paths_deleted(paths):
    for path in paths:
        if os.path.exists(path):
            print(f"{path} not deleted")


def check_lib_name(libname):
    if not os.path.exists(libname):
        print(f"{libname} not built")


def build_and_clean(step, cmdbuild, cmdclean, libname, paths):
    # Build the project
    print(f"{step}")
    p = bnr.run(cmdbuild)
    if p.out != "":
        cmd = " ".join(cmdbuild)
        print(f"GPRBuild failed : \"{cmd}\"")
    # Check if the library has the proper name
    check_lib_name(f"files/lib1/{libname}")
    # Clean the project
    p = bnr.run(cmdclean)
    if p.out != "":
        cmd = " ".join(cmdbuild)
        print(f"GPRClean failed : \"{cmd}\"")
    # Check if every paths have been deleted
    check_paths_deleted(paths)


bnr = BuilderAndRunner()

dir_to_clean = [
    'files/obj',
    'files/obj1',
    'files/lib1',
    'files/lib1-ali',
    'files/lib1-src'
]

name = "undefined_name"
prefix = "undefined_prefix_"
suffix = ".undefined_suffix"
with open("files/lib1.gpr", "r") as fp:
    for line in fp:
        if "library project" in line:
            name = re.search('library project (.*) is', line).group(1).lower()

# Generate the alternative configuration project
p = bnr.run(['gprconfig', '--batch', '--config=Ada', '-o', 'files/alternative.cgpr'])
replaced_content = ""
with open("files/alternative.cgpr", "r") as fp:
    for line in fp:
        new_line = line.strip("\n")
        if "Shared_Library_Prefix" in line:
            new_line = re.sub('"(.*)"', '"some_prefix_"', line.strip())
        elif "Shared_Library_Suffix" in line:
            new_line = re.sub('"(.*)"', '".some_suffix"', line.strip())
        replaced_content = replaced_content + new_line + "\n"
with open("files/alternative.cgpr", "w") as fp:
    fp.write(replaced_content)

# Get the attributes value from the configuration project
with open("files/alternative.cgpr", "r") as fp:
    for line in fp:
        if "Shared_Library_Prefix" in line:
            prefix = re.search('"(.*)"', line).group(1)
        elif "Shared_Library_Suffix" in line:
            suffix = re.search('"(.*)"', line).group(1)

# Build and clean the project without configuration
build_and_clean(
    "Step 1 :",
    ['gprbuild', '-p', '-q', '-Pfiles/test.gpr'],
    [GPRCLEAN, '-r', '-p', '-q', '-Pfiles/test.gpr'],
    f"lib{name}.so",
    dir_to_clean
)

# Build and clean the project with configuration
build_and_clean(
    "Step 2 :",
    ['gprbuild', '-p', '-q', '-Pfiles/test.gpr', '--config=files/alternative.cgpr'],
    [GPRCLEAN, '-r', '-p', '-q', '-Pfiles/test.gpr', '--config=files/alternative.cgpr'],
    f"{prefix}{name}{suffix}",
    dir_to_clean
)

# Get the attributes value from the library project
name = "undefined_name"
prefix = "undefined_prefix_"
suffix = ".undefined_suffix"
with open("files/lib2.gpr", "r") as fp:
    for line in fp:
        if "library project" in line:
            name = re.search('library project (.*) is', line).group(1).lower()
        elif "Shared_Library_Prefix" in line:
            prefix = re.search('"(.*)"', line).group(1)
        elif "Shared_Library_Suffix" in line:
            suffix = re.search('"(.*)"', line).group(1)

# Build and clean the project without configuration but with attributes directly
# set in the library project
build_and_clean(
    "Step 3 :",
    ['gprbuild', '-p', '-q', '-Pfiles/test2.gpr'],
    [GPRCLEAN, '-r', '-p', '-q', '-Pfiles/test2.gpr'],
    f"{prefix}{name}{suffix}",
    dir_to_clean
)

# Build and clean the project with configuration but with attributes directly
# set in the library project. Test the overloading of the attributes.
build_and_clean(
    "Step 4 :",
    ['gprbuild', '-p', '-q', '-Pfiles/test2.gpr', '--config=files/alternative.cgpr'],
    [GPRCLEAN, '-r', '-p', '-q', '-Pfiles/test2.gpr',
     '--config=files/alternative.cgpr'],
    f"{prefix}{name}{suffix}",
    dir_to_clean
)
