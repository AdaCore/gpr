import re
from testsuite_support.builder_and_runner import BuilderAndRunner, GPRINSPECT

bnr = BuilderAndRunner()

p = bnr.run([GPRINSPECT, "-Proot.gpr", "--all", "-r"])

output = p.out

output = re.sub(r"(Generated on *:) .*", r"\1 2000-01-01 00:00:00", output)
output = re.sub(r"(Version *:) .*", r"\1 Pro 1.0 (20000101) (<host>)", output)
output = re.sub(r"( *- )/.*ada(lib|include/)", r"\1<gnat rts>/ada\2", output)
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
