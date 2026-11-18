The ALI fixtures in `tree/obj` were generated with GNAT Pro 28.0w 20261115 (GCC
16.2.1 20261116)

Regenerate from this test directory with:

```sh
gprbuild -p -q -c -U -Ptree/prj.gpr
```

`token_spacing.ali` is a spacing variant of the generated `main.ali`.
It preserves compiler-produced checksums and unit records, changes the
separator after U/W/Y/Z unit names to one space, and uses the legacy
unquoted spelling of `file with spaces.ads`. Regenerate the variant with:

```sh
python3 - <<'PYTHON'
from pathlib import Path
import re

text = Path("tree/obj/main.ali").read_text()
text = re.sub(r"^([UWYZ] \S+)[ \t]+", r"\1 ", text, flags=re.MULTILINE)
text = re.sub(
    r'^D "?file with spaces\.ads"?[ \t]+',
    "D file with spaces.ads  ", text, flags=re.MULTILINE,
)
Path("tree/obj/token_spacing.ali").write_text(text)
PYTHON
```
