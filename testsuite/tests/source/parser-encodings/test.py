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
checks = []


def check(mode, filename, expected):
    checks.append((mode, filename, expected))


def validate(mode, filename, expected, lines):
    global count
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

bracket_names = [
    ("short", 'Caf["E9"]', "Café"),
    ("four", '["00C9"]t["00e9"]', "Été"),
    ("six", '["010428"]', "𐐨"),
    ("eight", '["00010428"]', "𐐨"),
    ("adjacent", '["00E9"]["0100"]["5B57"]', "éĀ字"),
    ("ascii", 'C["0061"]fe', "Cafe"),
    ("mixed-latin1", 'Café["0100"]', "CaféĀ"),
    ("mixed-unicode", 'Ā["00E9"]𐐨', "Āé𐐨"),
]
hex_names = [
    ("short", 'Caf\x1b00e9', "Café"),
    ("start", '\x1b00C9te', "Éte"),
    ("greek", '\x1b03B1lpha', "αlpha"),
    ("wide", '\x1b5B57', "字"),
    ("adjacent", '\x1b00E9\x1b0100\x1b5B57', "éĀ字"),
    ("ascii", 'C\x1b0061fe', "Cafe"),
    ("mixed-latin1", 'Café\x1b0100', "CaféĀ"),
    ("mixed-brackets", '\x1b00C9["0100"]', "ÉĀ"),
]
encoded_names = ([('brackets', *name) for name in bracket_names]
                 + [('hex', *name) for name in hex_names])
for notation, kind, name, decoded in encoded_names:
    cases = [
        ("with", f'with {name} -- ["not hex"] \x1bGGGG\n'
         ' . ["5B57"], Other; '
         'procedure Example is begin null; end;',
         ["unit=Example", f"with={decoded}.字", "with=Other"]),
        ("unit", f'package body {name} . ["0100"] is end;',
         [f"unit={decoded}.Ā"]),
        ("separate", f'separate ({name} . ["5B57"]) '
         'procedure ["00C9"] is begin null; end;',
         ["unit=É", f"parent={decoded}.字"]),
    ]
    if notation == "hex":
        cases.append(("simple-unit", f'procedure {name} is begin null; end;',
                      [f"unit={decoded}"]))
    for context, source, expected in cases:
        for label, encoding, bom in [("latin1", "latin-1", b"")] + encodings:
            try:
                data = source.encode(encoding)
            except UnicodeEncodeError:
                continue
            filename = f"{notation}-{kind}-{context}-{label}.adb"
            Path(filename).write_bytes(bom + data)
            for mode in ("callback", "mapped"):
                check(mode, filename, expected)
            if encoding == "utf-8":
                check("reader", filename, expected)

for label, digits in [
    ("empty", ""),
    ("one", "0"),
    ("two", "00"),
    ("three", "000"),
    ("hex", "00GG"),
    ("last-digit", "123G"),
    ("high-surrogate", "D800"),
    ("low-surrogate", "DEAD"),
]:
    for position, prefix in [("start", ""), ("middle", "Bad")]:
        filename = f"hex-invalid-{label}-{position}.adb"
        Path(filename).write_bytes(f"procedure {prefix}\x1b{digits}".encode())
        for mode in ("callback", "mapped", "reader"):
            check(mode, filename, ["ERROR"])

for label, name in [
    ("empty", 'Bad[""]'),
    ("odd", 'Bad["123"]'),
    ("long", 'Bad["123456789"]'),
    ("hex", 'Bad["GGGG"]'),
    ("unclosed", 'Bad["00E9"'),
    ("surrogate", 'Bad["DEAD"]'),
    ("range", 'Bad["00110000"]'),
    ("overflow", 'Bad["FFFFFFFF"]'),
]:
    filename = f"brackets-invalid-{label}.adb"
    Path(filename).write_text(f"procedure {name} is end;", encoding="utf-8")
    for mode in ("callback", "mapped", "reader"):
        check(mode, filename, ["ERROR"])

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

# Run the full matrix in one process to amortize Valgrind initialization.
Path("cases.txt").write_text("".join(
    f"{mode}\n{filename}\n" for mode, filename, _ in checks), encoding="utf-8")
result = bnr.run(["./main", "batch", "cases.txt"], output="run.out")
assert result.status == 0, f"batch command exited with status {result.status}"
output = iter(Path("run.out").read_text(encoding="utf-8").splitlines())
for mode, filename, expected in checks:
    lines = []
    for line in output:
        if line == "END":
            break
        lines.append(line)
    else:
        raise AssertionError((mode, filename, "missing result terminator", lines))
    validate(mode, filename, expected, lines)
remaining = list(output)
assert not remaining, remaining
print(f"{count} encoding checks passed")
