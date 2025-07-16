from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD
from e3.env import Env
import shutil
import os
import os.path

bnr = BuilderAndRunner()

def clear(directory):
    print(f"   - Clearing \"{directory}\"")
    shutil.rmtree(os.path.join(os.getcwd(), directory))

def generate_makefile(quantity, root_dirname):

    if os.path.exists(os.path.join(os.getcwd(), 'Makefile')):
        os.remove(os.path.join(os.getcwd(), 'Makefile'))

    print(f"   - Creating \"Makefile\"")
    with open(os.path.join(os.getcwd(), "Makefile"), "w+") as f:
        f.write(".PHONY: clean all")
        for i in range(1, quantity + 1):
            f.write(f" build{i}")

        f.write("\n\nclean:\n")
        for i in range(1, quantity + 1):
            f.write(f"\tgprclean -P {os.path.join(os.getcwd(), f'{root_dirname}{i}', 'prj.gpr')}\n")

        f.write("\nall:")
        for i in range(1, quantity + 1):
            f.write(f" build{i}")
        f.write("\n\n")

        for i in range(1, quantity + 1):
            f.write(f"build{i}:\n")
            f.write(f"\t+{GPRBUILD} -P {os.path.join(os.getcwd(), f'{root_dirname}{i}', 'prj.gpr')} -j0\n\n")

def generate_sources(directory, quantity):
    print(f"   - Creating \"{directory}\"")
    os.makedirs(os.path.join(os.getcwd(), directory))

    for j in range(1, quantity + 1):
        with open(os.path.join(os.getcwd(), directory, f"pack_{j}.ads"), "w+") as f:
            f.write(f"package Pack_{j} is end;")

    with open(os.path.join(os.getcwd(), directory, "prj.gpr"), "w+") as f:
        f.write("project Prj is\n")
        f.write("end Prj;\n")

print("Step 1 - Build a single project")
print("- Generate \"src1\", 10 files")
generate_sources("src1", 10)
print("- Generate makefile")
generate_makefile(1, "src")
print("- Build")
bnr.run(['make', 'all', '-j2'])
print("- Rebuild")
bnr.run(['make', 'all', '-j2'])
print("- Clean")
bnr.run(['make', 'clean'])
print("Step 2 - Build two projects")
print("- Generate \"src2\", 10 files")
generate_sources("src2", 10)
print("- Generate makefile")
generate_makefile(2, "src")
print("- Build")
bnr.run(['make', 'all', '-j3'])
print("- Rebuild")
bnr.run(['make', 'all', '-j3'])
print("- Clear sources")
clear("src1")
clear("src2")
print("Step 3 - Build a bigger single project")
print("- Generate \"src1\", 3000 files")

if Env().host.platform.endswith('windows') or Env().host.platform.endswith('windows64'):
	generate_sources("src1", 300)
else:
	generate_sources("src1", 3000)

print("- Generate makefile")
generate_makefile(1, "src")
print("- Build")
bnr.run(['make', 'all', '-j20'])
print("- Rebuild")
bnr.run(['make', 'all', '-j20'])
print("Step 4 - Build bigger projects")
print("- Generate \"src2\", 3000 files")

if Env().host.platform.endswith('windows') or Env().host.platform.endswith('windows64'):
	generate_sources("src2", 300)
else:
	generate_sources("src2", 3000)

print("- Generate makefile")
generate_makefile(2, "src")
print("- Build")
bnr.run(['make', 'all', '-j20'])
print("- Rebuild")
bnr.run(['make', 'all', '-j20'])
print("- Clean")
bnr.run(['make', 'clean'])
