import glob
import os

from e3.env import Env

from testsuite_support.builder_and_runner import BuilderAndRunner

bnr = BuilderAndRunner()
bnr.build("test.gpr", args=["-p", "-q"])

on_windows = Env().host.os.name == "windows"


def create(path):
    os.makedirs(os.path.dirname(path), exist_ok=True)
    with open(path, "w") as f:
        f.write("content\n")


# The artifact has to exist: the signature records its checksum
art = os.path.abspath(os.path.join("tree", "obj", "artifact.o"))
create(art)

paths = [art]

if on_windows:
    paths.append(art.replace("\\", "/"))
    paths.append("\\\\?\\" + art)

    # UNC forms, through the drive's administrative share when reachable
    drive, rest = os.path.splitdrive(art)
    share = "localhost\\" + drive[0] + "$" + rest
    if os.path.exists("\\\\" + share):
        paths.append("\\\\" + share)
        paths.append("\\\\?\\UNC\\" + share)
else:
    # On Unix a backslash is a regular file name character, kept as is
    odd = os.path.join(os.path.dirname(art), "back\\slash.o")
    create(odd)
    paths.append(odd)

errors = bnr.run(["./main"] + paths).out

if on_windows:
    # Separators are stored as '/', so no path needs any JSON escape
    for sig in sorted(glob.glob("sig*.json")):
        with open(sig) as f:
            if "\\" in f.read():
                errors += "KO: backslash stored in " + sig + "\n"

print(errors if errors else "OK")
