import json
import os
import re
from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRINSPECT

bnr = BuilderAndRunner()


def test(cmd):

    p = bnr.run(cmd)

    output = p.out

    # Remove escape char added on json output
    output = output.replace("\\\\", "\\")

    # Make sure build-specific values are normalized for test output comparison
    # purpose
    output = re.sub(r"(Generated on *:) .*", r"\1 2000-01-01 00:00:00", output)
    output = re.sub(r"(generated-on.*:) .*", r"\1 2000-01-01 00:00:00", output)
    output = re.sub(r"(Version *:) .*", r"\1 Pro 1.0 (20000101) (<host>)", output)
    output = re.sub(r"(version.*:) .*", r"\1 Pro 1.0 (20000101) (<host>)", output)
    output = re.sub(r"(.*value.*GNAT ).*", r"\1<version>", output)
    output = re.sub(r"(.*Compiler_Version[^\n]*\s*- Value : ).*", r"\1<version>", output)
    output = output.replace(os.getcwd(), "<cwd>")
    # json mode uses // as path separator
    output = output.replace(re.sub(r"//", r"/", os.getcwd()), "<cwd>")

    # Windows paths encoded in view_ids:
    # in view_ids, all paths are lower-cased (case insensitive filesystem)
    if os.name == 'nt':
        output = output.replace(os.getcwd().lower(), "<cwd>")

    # now for all hosts, make sure the runtime path is normalized for test
    # output comparison purpose
    output = re.sub(r"( *- ).*([/\\]ada(lib|include))(.*)", r"\1<gnat rts>\2\4", output)
    output = re.sub(r"( *\").*([/\\]ada(lib|include))(.*)", r"\1<gnat rts>\2\4", output)
    output = re.sub(r"( *\"value\": \").*([/\\](gprbuild[/\\]gprbind|bin[/\\]gcc|lib[/\\]gcc))(.*)",
                    r"\1<root>\2\"", output)
    out = []
    in_path = False
    in_textual_path = False
    in_json_path = False

    for line in output.splitlines():
        if line.startswith("* Project search paths"):
            out.append(line)
            in_path = True
            in_textual_path = True
            out.append("    - <filtered-out>")
        elif "project-search-paths" in line:
            out.append(line)
            in_path = True
            in_json_path = True
            out.append("      <filtered-out>")
        elif in_textual_path and line.startswith("* "):
            in_path = False
            in_textual_path = False
        elif in_json_path and line.startswith("    ]"):
            in_path = False
            in_json_path = False
        if not in_path:
            out.append(line)

    output = '\n'.join(out)
    print(output)


test([GPRINSPECT, "-Proot.gpr", "--all", "-r"])
test([GPRINSPECT, "-Proot.gpr", "--all", "-r", "--display=json"])

print("Check JSON validity:")

JSON_File = os.path.join(os.getcwd(), "output.json")

p = bnr.run([GPRINSPECT, "-Proot.gpr", "--all", "-r", "--display=json"], output=JSON_File)

with open(JSON_File) as jfile:
    try:
        json.load(jfile)
        print ("   Valid JSON output")
    except ValueError as e:
        print(f"   Invalid JSON output: {e}")

print("Check JSON validity for default project:")

old = os.getcwd()
os.chdir(os.path.join(old, "with_default"))
p = bnr.run([GPRINSPECT, "--all", "-r", "--display=json"], output=JSON_File)
os.chdir(old)

with open(JSON_File) as jfile:
    try:
        json.load(jfile)
        print ("   Valid JSON output")
    except ValueError as e:
        print(f"   Invalid JSON output: {e}")

print("Check JSON validity with error:")

# several projects in current directory, so no default is loaded and an error
# is reported

p = bnr.run([GPRINSPECT, "--all", "-r", "--display=json"], output=JSON_File)

with open(JSON_File) as jfile:
    try:
        json.load(jfile)
        print ("   Valid JSON output")
    except ValueError as e:
        print(f"   Invalid JSON output : {e}")
