import os
import re
from testsuite_support.builder_and_runner import BuilderAndRunner, GPRINSPECT

bnr = BuilderAndRunner()

p = bnr.run([GPRINSPECT, "-Proot.gpr", "--all", "-r"])

output = p.out

# Make sure build-specific values are normalized for test output comparison
# purpose
output = re.sub(r"(Generated on *:) .*", r"\1 2000-01-01 00:00:00", output)
output = re.sub(r"(Version *:) .*", r"\1 Pro 1.0 (20000101) (<host>)", output)
output = output.replace(os.getcwd(), "<cwd>")

# Windows paths encoded in view_ids:
# in view_ids, all paths are lower-cased (case insensitive filesystem)
if os.name == 'nt':
    cwd = os.getcwd().lower()
    # first replace $PWD with a generic value (backslashes as path separators on
    # both sides)
    output = output.replace(cwd, "<cwd>")
    # for the other paths, replace backslashes with slashes
    output = output.replace("\\", "/")
    # check windows drive, and remove it to have "root" paths only
    output = re.sub(r"(.*)[a-zA-Z]://", r"\1/", output)

# now for all hosts, make sure the runtime path is normalized for test
# output comparison purpose
output = re.sub(r"( *- ).*/ada(lib|include)(.*)", r"\1<gnat rts>/ada\2\3", output)
out = []
in_path = False

for line in output.splitlines():
    if line.startswith("* Project search paths"):
        out.append(line)
        in_path = True
        out.append("    - <filtered-out>")
    elif in_path and line.startswith("* "):
        in_path = False
    if not in_path:
        out.append(line)

output = '\n'.join(out)
print(output)
