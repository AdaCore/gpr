from pathlib import Path

from testsuite_support.builder_and_runner import BuilderAndRunner


fold = {}
mentioned = set(range(128))
for line in Path("CaseFolding.txt").read_text(encoding="utf-8").splitlines():
    fields = line.split("#", 1)[0].split(";")
    if len(fields) < 3:
        continue
    code = int(fields[0], 16)
    mentioned.add(code)
    if fields[1].strip() in ("C", "S"):
        target = int(fields[2], 16)
        mentioned.add(target)
        fold[code] = target
Path("cases.txt").write_text("".join(
    f"{code:X} {fold.get(code, code):X}\n" for code in sorted(mentioned)))

tree = Path("tree")
tree.mkdir()
units = [("alpha", "Α", "α"), ("sigma", "Σ", "ς"),
         ("stroke", "Ⱥ", "ⱥ")]
naming = []
for filename, declared, source in units:
    naming.append(f'      for Spec ("{declared}") use "{filename}.ads";')
    (tree / f"{filename}.ads").write_text(
        f"package {source} is end {source};", encoding="utf-8-sig")
(tree / "units.gpr").write_text(
    'project Units is\n   for Languages use ("Ada");\n'
    '   package Naming is\n' + "\n".join(naming)
    + '\n   end Naming;\nend Units;\n', encoding="utf-8")

bnr = BuilderAndRunner()
bnr.build("test.gpr", args=["-p", "-q"])
bnr.check_call(["./test", "cases.txt"])
