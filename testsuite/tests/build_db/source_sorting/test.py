from pathlib import Path

from testsuite_support.builder_and_runner import BuilderAndRunner

bnr = BuilderAndRunner()
for project, files in {"a": ["Alpha.c", "Zeta.c", "bee.c"],
                       "b": ["alpha.c", "Beta.c"]}.items():
    directory = Path("tree") / project
    directory.mkdir(parents=True)
    for filename in files:
        (directory / filename).write_text("int synthetic;\n")
    (Path("tree") / (project + ".gpr")).write_text(
        'project ' + project + ' is\n'
        '   for Languages use ("C");\n'
        '   for Source_Dirs use ("' + project + '");\n'
        '   for Object_Dir use "obj_' + project + '";\n'
        'end ' + project + ';\n')
(Path("tree") / "root.gpr").write_text(
    'with "a"; with "b";\nproject Root is\n'
    '   for Languages use ("C");\n'
    '   for Source_Dirs use ();\n'
    '   for Object_Dir use "obj_root";\nend Root;\n')

bnr.build("test.gpr", args=["-q", "-p"])
bnr.call(["./main"])
