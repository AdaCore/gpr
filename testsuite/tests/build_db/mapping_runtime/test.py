import json
import re
import shlex
import shutil
import sys
from pathlib import Path

from e3.env import Env

from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD, GPRCONFIG

bnr = BuilderAndRunner()
runtime = Path(bnr.check_output(["gcc", "-print-file-name=adainclude"]).out.strip())
for directory in ("src", "override"):
    Path(directory).mkdir()
for extension in ("ads", "adb"):
    filename = "s-assert." + extension
    shutil.copy2(runtime / filename, Path("override") / filename)
Path("src/main.adb").write_text(
    "with System.Assertions;\n"
    "procedure Main is begin null; end Main;\n")
# Capture each fresh mapping before GNAT adds entries for runtime dependencies.
Path("compiler.py").write_text(
    "import pathlib, shutil, subprocess, sys\n"
    "for arg in sys.argv[1:]:\n"
    "    if arg.startswith('-gnatem='):\n"
    "        source = pathlib.Path(arg[len('-gnatem='):])\n"
    "        snapshot = pathlib.Path(str(source) + '.initial')\n"
    "        if not snapshot.exists():\n"
    "            shutil.copyfile(source, snapshot)\n"
    "sys.exit(subprocess.call([" + repr(shutil.which("gcc")) +
    "] + sys.argv[1:]))\n")
compiler = Path("compiler.py").resolve().as_posix().replace('"', '""')
python = Path(sys.executable).as_posix().replace('"', '""')
Path("test.gpr").write_text(
    'project Test is\n'
    '   for Languages use ("Ada");\n'
    '   for Source_Dirs use ("src", "override");\n'
    '   for Object_Dir use "obj";\n'
    '   package Compiler is\n'
    '      for Driver ("Ada") use "' + python + '";\n'
    '      for Default_Switches ("Ada") use\n'
    '        ("-gnatg", "-gnatws", "-gnatyN");\n'
    '   end Compiler;\n'
    'end Test;\n')
bnr.check_output([GPRCONFIG, "--batch", "--config=Ada",
                  "-o", "compiler.cgpr"])
config = Path("compiler.cgpr")
text, count = re.subn(
    r'(for Leading_Required_Switches\s*\("Ada"\)\s*use\s*\()',
    lambda match: match[0] + '"' + compiler + '", ',
    config.read_text(), flags=re.IGNORECASE)
assert count == 1, "compiler leading switches not found"
config.write_text(text)
bnr.check_output([GPRBUILD, "-Ptest.gpr", "--config=compiler.cgpr",
                  "-p", "-q", "-c", "-U", "-j2",
                  "--json-summary", "--keep-temp-files"])
files = set()
for action in json.loads(Path("jobs.json").read_text()):
    for arg in shlex.split(action["command"],
                           posix=Env().host.os.name != "windows"):
        arg = arg.strip('"')
        if arg.startswith("-gnatem="):
            files.add(Path(arg[len("-gnatem="):]))
assert files, "no mapping files produced"
for file in files:
    snapshot = Path(str(file) + ".initial")
    lines = snapshot.read_text().splitlines()
    assert len(lines) % 3 == 0, "incomplete mapping record"
    records = [lines[i:i + 3] for i in range(0, len(lines), 3)]
    paths = {Path(record[2]).resolve() for record in records}
    assert paths == {Path("src/main.adb").resolve(),
                     Path("override/s-assert.ads").resolve(),
                     Path("override/s-assert.adb").resolve()}, paths
    snapshot.unlink()
print("OK")
