import codecs
from pathlib import Path

from testsuite_support.builder_and_runner import BuilderAndRunner

bnr = BuilderAndRunner()
bnr.build("test.gpr", args=["-p"])

sources = {
    "latin1": ("with Café . Élan; procedure Été is begin null; end Été;",
               "latin-1", b"", ["unit=Été", "with=Café.Élan"]),
    "unicode": ("with Café . 字; procedure éā字𐐨 is begin null; end éā字𐐨;",
                None, None, ["unit=éā字𐐨", "with=Café.字"]),
    "separate": ("separate (Café . 字) procedure Ā is begin null; end Ā;",
                 None, None, ["unit=Ā", "parent=Café.字"]),
}
encodings = [
    ("utf8", "utf-8", codecs.BOM_UTF8),
    ("utf16le", "utf-16-le", codecs.BOM_UTF16_LE),
    ("utf16be", "utf-16-be", codecs.BOM_UTF16_BE),
    ("utf32le", "utf-32-le", codecs.BOM_UTF32_LE),
    ("utf32be", "utf-32-be", codecs.BOM_UTF32_BE),
]

count = 0

def check(mode, filename, expected):
    global count
    bnr.run(["./main", mode, filename], output="run.out")
    lines = Path("run.out").read_text(encoding="utf-8").splitlines()
    # Compute also registers the parent of each withed child unit.

    def dependency_case(line):
        return line.casefold() if line.startswith("with=") else line

    def unicode_upper(c):
        upper = c.upper()
        return upper if len(upper) == 1 else c

    def expected_case(line):
        if mode != "callback" and line.startswith(("unit=", "parent=")):
            prefix, separator, name = line.partition("=")
            line = prefix + separator + "".join(unicode_upper(c) for c in name)
        return dependency_case(line)

    actual = [dependency_case(line) for line in lines]
    required = [expected_case(line) for line in expected]
    assert all(line in actual for line in required), (mode, filename, lines)
    if expected == ["ERROR"]:
        assert lines == ["ERROR"], (filename, lines)
    else:
        assert all(line.startswith(("unit=", "with=", "parent="))
                   for line in lines), (filename, lines)
    count += 1

for kind, (source, encoding, bom, expected) in sources.items():
    cases = [(kind, encoding, bom)] if encoding else encodings
    for label, encoding, bom in cases:
        filename = f"{kind}-{label}.adb"
        Path(filename).write_bytes(bom + source.encode(encoding))
        for mode in ("callback", "mapped"):
            check(mode, filename, expected)
        if encoding == "utf-8":
            check("reader", filename, expected)

separator_sources = {
    "with": (["with", "Café", ".", "Élan", ",", "Other", ";",
              "procedure", "Été", "is begin null; end Été;"],
             ["unit=Été", "with=Café.Élan", "with=Other"]),
    "unit": (["package", "body", "Café", ".", "Élan",
              "is end Café.Élan;"], ["unit=Café.Élan"]),
    "separate": (["separate", "(", "Café", ".", "Élan", ")",
                  "procedure", "Été", "is begin null; end Été;"],
                 ["unit=Été", "parent=Café.Élan"]),
}
for separator_label, separator in [("vt", "\v"), ("ff", "\f")]:
    for kind, (tokens, expected) in separator_sources.items():
        source = separator.join(tokens)
        for label, encoding, bom in [("latin1", "latin-1", b"")] + encodings:
            filename = f"separators-{kind}-{separator_label}-{label}.adb"
            Path(filename).write_bytes(bom + source.encode(encoding))
            for mode in ("callback", "mapped"):
                check(mode, filename, expected)
            if encoding == "utf-8":
                check("reader", filename, expected)

comment_sources = {
    "with": ([
        "--\twith Wrong; procedure Hidden is",
        "with Café-- before dot",
        "-- another comment",
        ".-- after dot",
        "-- yet another comment",
        "Élan-- before comma",
        ", Other-- before semicolon",
        "; -- with Hidden;",
        "procedure Été is begin null; end Été;",
    ], ["unit=Été", "with=Café.Élan", "with=Other"]),
    "unit": ([
        "package body Café-- before dot",
        ".-- after dot",
        "Élan-- after name",
        "is end Café.Élan;",
    ], ["unit=Café.Élan"]),
    "separate": ([
        "separate (Café-- before dot",
        ".-- after dot",
        "Élan-- after name",
        ") procedure Été is begin null; end Été;",
    ], ["unit=Été", "parent=Café.Élan"]),
}
for ending, newline in [("lf", "\n"), ("cr", "\r"),
                        ("crlf", "\r\n"), ("lfcr", "\n\r"),
                        ("vt", "\v"), ("ff", "\f")]:
    for kind, (lines, expected) in comment_sources.items():
        source = newline.join(lines)
        for label, encoding, bom in [("latin1", "latin-1", b"")] + encodings:
            filename = f"comments-{kind}-{ending}-{label}.adb"
            Path(filename).write_bytes(bom + source.encode(encoding))
            for mode in ("callback", "mapped"):
                check(mode, filename, expected)
            if encoding == "utf-8":
                check("reader", filename, expected)

for label, encoding, bom in encodings:
    filename = f"ascii-{label}.adb"
    Path(filename).write_bytes(bom + b"".join(
        c.encode(encoding) for c in "procedure Example is begin null; end;"))
    check("mapped", filename, ["unit=Example"])

for label, data in [
    ("utf16-short", codecs.BOM_UTF16_LE + b"x"),
    ("utf16-surrogate", codecs.BOM_UTF16_BE + b"\xd8\x00"),
    ("utf32-short", codecs.BOM_UTF32_LE + b"x"),
    ("utf32-invalid", codecs.BOM_UTF32_BE + b"\x00\x11\x00\x00"),
    ("utf8-invalid", codecs.BOM_UTF8 + b"procedure Bad\xff is end;"),
    ("utf16-bom-only", codecs.BOM_UTF16_LE),
    ("utf32-bom-only", codecs.BOM_UTF32_BE),
    ("empty", b""),
]:
    filename = label + ".adb"
    Path(filename).write_bytes(data)
    for mode in ("callback", "mapped"):
        check(mode, filename, ["ERROR"])

check("reader-error", "unicode-utf8.adb", ["ERROR"])
tree = Path("tree")
tree.mkdir()
source = "procedure Éā字𐐨 is begin null; end Éā字𐐨;"
(tree / "main.adb").write_bytes(codecs.BOM_UTF8 + source.encode("utf-8"))
Path("tree.gpr").write_text(
    'project Tree is for Source_Dirs use ("tree"); '
    'package Naming is for Body ("éā字𐐨") use "main.adb"; '
    'end Naming; end Tree;', encoding="utf-8")
check("tree", "tree.gpr", ["unit=éā字𐐨"])
print(f"{count} encoding checks passed")
