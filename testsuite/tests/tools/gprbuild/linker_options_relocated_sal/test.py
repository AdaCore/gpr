import glob
import os
import re
import shutil

from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD, GPRINSTALL

bnr = BuilderAndRunner()

FAILURES = []


def check(condition, message):
    if not condition:
        FAILURES.append(message)


# ---------------------------------------------------------------------------
# 1. Build the SAL normally, in its original location ("machine A").
# ---------------------------------------------------------------------------

bnr.simple_run([GPRBUILD, "-P" + os.path.join("tree", "lib.gpr"), "-p", "-q"])

archive = os.path.join("tree", "lib", "libgreeter.a")

check(
    os.path.isfile(archive),
    "FAIL: library archive was not produced at " + archive,
)

# ---------------------------------------------------------------------------
# 1b. Control check: does a plain, directly-withed, non-relocated,
#     non-installed main even work against this SAL? Run this before any of
#     the gprinstall/relocation machinery below. If this step ever fails,
#     the problem is in basic SAL/auto-init support, not in relocation or in
#     the linker-options portability fix -- don't waste time on the steps
#     below until this one passes on its own.
# ---------------------------------------------------------------------------

bnr.simple_run(
    [GPRBUILD, "-P" + os.path.join("control", "control_main.gpr"), "-p", "-q"]
)
control_result = bnr.simple_run([os.path.join("control", "obj", "control_main")])

check(
    "hello from relocated SAL" in control_result.out,
    "FAIL: even a direct, non-relocated, non-installed main fails against "
    "this SAL -- this is a basic SAL-consumption problem, not a relocation "
    "or linker-options issue:\n" + control_result.out,
)

# ---------------------------------------------------------------------------
# 2. Regression check on the write side.
#
#    Dump the embedded .GPR.linker_options section and make sure -lgnat and
#    -lgnarl are still bare, portable tokens -- not this machine's absolute
#    adalib path. This directly targets Ada_Bind.Post_Execution: it must feed
#    Link_Options_Insert the raw token gnatbind emitted, and resolve to an
#    absolute path only for the in-tree Link action, never for what gets
#    embedded in the archive.
# ---------------------------------------------------------------------------

def decode_objdump_section(objdump_out):
    """Decode the hex+ASCII dump produced by `objdump -s --section=...`
    into the actual bytes of the section, then split into lines. Needed
    because the raw dump never contains a token like '-lgnat' on a line
    by itself -- it's always interleaved with addresses and hex columns,
    and often wrapped mid-string across objdump's fixed 16-bytes-per-line
    layout.
    """
    data = bytearray()
    for line in objdump_out.splitlines():
        parts = line.split()
        if not parts or not re.fullmatch(r"[0-9a-f]{1,8}", parts[0]):
            # Skip headers, blank lines, "In archive ...", "Contents of
            # section ...", "<member>: file format ..." etc: only lines
            # starting with a hex address are actual dump content.
            continue
        for group in parts[1:5]:
            if re.fullmatch(r"[0-9a-f]+", group) and len(group) % 2 == 0:
                data += bytes.fromhex(group)
    return [line for line in data.decode("latin1").split("\n") if line]


objdump_out = bnr.simple_run(
    ["objdump", "-s", "--section=.GPR.linker_options", archive]
).out

section_lines = decode_objdump_section(objdump_out)

check(
    "-lgnat" in section_lines,
    "FAIL: -lgnat missing or not bare in the embedded linker_options section",
)
# -lgnarl only appears if the library actually pulls in tasking; a plain
# program like Greeter legitimately never gets one. Don't require its
# presence, only that it's bare *if* present.
if any("gnarl" in line for line in section_lines):
    check(
        "-lgnarl" in section_lines,
        "FAIL: -lgnarl present but not bare in the embedded linker_options "
        "section",
    )
check(
    not any(
        "libgnat.a" in line or "libgnarl.a" in line for line in section_lines
    ),
    "FAIL: an absolute runtime archive path leaked into the linker_options "
    "section (the bug this test guards against)",
)

# ---------------------------------------------------------------------------
# 2b. Same write-side check, but with a fixture that actually exercises
#     -lgnarl. Greeter has no tasking dependency, so the check above never
#     verifies the -lgnarl branch of the fix at all. This one does, via a
#     protected object that forces the tasking runtime in.
# ---------------------------------------------------------------------------

bnr.simple_run(
    [GPRBUILD, "-P" + os.path.join("tree_tasking", "worker_tasking_lib.gpr"),
     "-p", "-q"]
)

tasking_archive = os.path.join("tree_tasking", "lib", "libworker_tasking.a")

tasking_objdump_out = bnr.simple_run(
    ["objdump", "-s", "--section=.GPR.linker_options", tasking_archive]
).out

tasking_section_lines = decode_objdump_section(tasking_objdump_out)

check(
    "-lgnat" in tasking_section_lines,
    "FAIL: -lgnat missing or not bare in the tasking fixture's embedded "
    "linker_options section",
)
check(
    "-lgnarl" in tasking_section_lines,
    "FAIL: -lgnarl missing or not bare in the tasking fixture's embedded "
    "linker_options section (this is the branch Greeter alone never "
    "exercises)",
)
check(
    not any(
        "libgnat.a" in line or "libgnarl.a" in line
        for line in tasking_section_lines
    ),
    "FAIL: an absolute runtime archive path leaked into the tasking "
    "fixture's linker_options section",
)

# ---------------------------------------------------------------------------
# 2c. Encapsulated SAL check. Encapsulated libraries fold the runtime in
#     using the _pic variants (libgnat_pic.a/libgnarl_pic.a) via a distinct
#     branch in Ada_Bind.Post_Execution. Neither the plain nor the _pic
#     absolute paths should ever be embedded in the archive.
# ---------------------------------------------------------------------------

bnr.simple_run(
    [GPRBUILD,
     "-P" + os.path.join("tree_encapsulated", "worker_encapsulated_lib.gpr"),
     "-p", "-q"]
)

encapsulated_archive = os.path.join(
    "tree_encapsulated", "lib", "libworker_encapsulated.a"
)

encapsulated_objdump_out = bnr.simple_run(
    ["objdump", "-s", "--section=.GPR.linker_options", encapsulated_archive]
).out

encapsulated_section_lines = decode_objdump_section(encapsulated_objdump_out)

check(
    not any(
        "libgnat.a" in line
        or "libgnarl.a" in line
        or "libgnat_pic.a" in line
        or "libgnarl_pic.a" in line
        for line in encapsulated_section_lines
    ),
    "FAIL: an absolute runtime archive path (regular or _pic) leaked into "
    "the encapsulated fixture's linker_options section",
)

# ---------------------------------------------------------------------------
# 2d. Tasking consumption round trip. The write-side checks above only
#     inspect the archive's static content; they never exercise
#     Post_Execution's resolution logic through a real link. This builds a
#     real consumer, via gprinstall + relocation like the main Greeter
#     scenario, and inspects the actual final link command.
# ---------------------------------------------------------------------------

tasking_install_prefix = os.path.abspath("install_tasking")
bnr.simple_run(
    [GPRINSTALL, "-p", "--prefix=" + tasking_install_prefix,
     os.path.join("tree_tasking", "worker_tasking_lib.gpr")]
)

tasking_relocated = os.path.abspath("install_tasking_relocated")
shutil.move(tasking_install_prefix, tasking_relocated)
os.rename("tree_tasking", "tree_tasking_moved_out_of_the_way")

tasking_env = dict(os.environ)
tasking_env["GPR_PROJECT_PATH"] = (
    os.path.join(tasking_relocated, "share", "gpr")
    + os.pathsep
    + tasking_env.get("GPR_PROJECT_PATH", "")
)

tasking_generated = glob.glob(
    os.path.join(tasking_relocated, "share", "gpr", "*.gpr")
)
assert len(tasking_generated) == 1, (
    "expected exactly one generated project under share/gpr, found: "
    + repr(tasking_generated)
)
tasking_generated_name = os.path.basename(tasking_generated[0])

with open(os.path.join("main_tasking", "main_tasking.gpr")) as f:
    content = f.read()
content = content.replace(
    'with "worker_tasking_lib.gpr";', 'with "%s";' % tasking_generated_name
)
with open(os.path.join("main_tasking", "main_tasking.gpr"), "w") as f:
    f.write(content)

tasking_build_out = bnr.simple_run(
    [GPRBUILD, "-P" + os.path.join("main_tasking", "main_tasking.gpr"),
     "-p", "-f", "-v"],
    env=tasking_env,
).out

check(
    re.search(r"adalib[/\\]libgnat\.a\b", tasking_build_out) is not None,
    "FAIL: tasking consumer's final link shows no resolved libgnat.a:\n"
    + tasking_build_out,
)
check(
    re.search(r"adalib[/\\]libgnarl\.a\b", tasking_build_out) is not None,
    "FAIL: tasking consumer's final link shows no resolved libgnarl.a -- "
    "this is the -lgnarl branch Greeter alone never exercises:\n"
    + tasking_build_out,
)

tasking_exe = os.path.join("main_tasking", "obj", "main_tasking")
tasking_result = bnr.simple_run([tasking_exe])
check(
    "hello from relocated SAL" in tasking_result.out,
    "FAIL: relocated tasking SAL did not link and run correctly:\n"
    + tasking_result.out,
)

# ---------------------------------------------------------------------------
# 2e. Encapsulated consumption round trip. This is the one case that can
#     actually distinguish "Post_Execution used the library's own view" from
#     the historical bug of silently using the consumer's view instead: a
#     plain executable is never Is_Library, so that bug and a correct fix
#     produce identical output for every non-encapsulated fixture above --
#     they only diverge here.
# ---------------------------------------------------------------------------

encapsulated_install_prefix = os.path.abspath("install_encapsulated")
bnr.simple_run(
    [GPRINSTALL, "-p", "--prefix=" + encapsulated_install_prefix,
     os.path.join("tree_encapsulated", "worker_encapsulated_lib.gpr")]
)

encapsulated_relocated = os.path.abspath("install_encapsulated_relocated")
shutil.move(encapsulated_install_prefix, encapsulated_relocated)
os.rename(
    "tree_encapsulated", "tree_encapsulated_moved_out_of_the_way"
)

encapsulated_env = dict(os.environ)
encapsulated_env["GPR_PROJECT_PATH"] = (
    os.path.join(encapsulated_relocated, "share", "gpr")
    + os.pathsep
    + encapsulated_env.get("GPR_PROJECT_PATH", "")
)

def ensure_library_standalone_encapsulated(generated_gpr_path):
    """gprinstall does not currently round-trip 'Library_Standalone' into
    the project it generates for an externally-built consumer -- it
    preserves Library_Kind/Library_Interface/Library_Name but silently
    drops Library_Standalone, so an encapsulated SAL's installed project
    resolves as an ordinary standalone library once installed. This is a
    gprinstall bug, separate from the Link_Options_Extract/Archive_Table_List
    fix this test otherwise targets. Work around it here so this test can
    still exercise the encapsulated resolution path in the meantime: insert
    the missing attribute directly into the generated project if it isn't
    already there.
    """
    with open(generated_gpr_path) as f:
        content = f.read()

    if "Library_Standalone" in content:
        return

    match = re.search(r"^end (\w+);\s*$", content, re.MULTILINE)
    assert match is not None, (
        "could not find the closing 'end <Project>;' line in "
        + generated_gpr_path + " to patch Library_Standalone into"
    )

    insertion = '   for Library_Standalone use "encapsulated";\n'
    patched = content[: match.start()] + insertion + content[match.start():]

    with open(generated_gpr_path, "w") as f:
        f.write(patched)


encapsulated_generated = glob.glob(
    os.path.join(encapsulated_relocated, "share", "gpr", "*.gpr")
)
assert len(encapsulated_generated) == 1, (
    "expected exactly one generated project under share/gpr, found: "
    + repr(encapsulated_generated)
)
encapsulated_generated_name = os.path.basename(encapsulated_generated[0])

ensure_library_standalone_encapsulated(encapsulated_generated[0])

with open(os.path.join("main_encapsulated", "main_encapsulated.gpr")) as f:
    content = f.read()
content = content.replace(
    'with "worker_encapsulated_lib.gpr";',
    'with "%s";' % encapsulated_generated_name,
)
with open(os.path.join("main_encapsulated", "main_encapsulated.gpr"), "w") as f:
    f.write(content)

encapsulated_build_out = bnr.simple_run(
    [GPRBUILD,
     "-P" + os.path.join("main_encapsulated", "main_encapsulated.gpr"),
     "-p", "-f", "-v"],
    env=encapsulated_env,
).out

check(
    re.search(r"adalib[/\\]libgnat_pic\.a\b", encapsulated_build_out)
    is not None,
    "FAIL: encapsulated consumer's final link shows no resolved "
    "libgnat_pic.a -- the resolution logic is not seeing the library's "
    "own view:\n" + encapsulated_build_out,
)
check(
    re.search(r"adalib[/\\]libgnarl_pic\.a\b", encapsulated_build_out)
    is not None,
    "FAIL: encapsulated consumer's final link shows no resolved "
    "libgnarl_pic.a:\n" + encapsulated_build_out,
)
# Note: plain libgnat.a/libgnarl.a legitimately also appear in this same
# link command -- they come from main_encapsulated's own bind (an
# ordinary, non-encapsulated executable needs the plain runtime for
# itself), entirely independent of Worker_Encapsulated_Lib's own
# resolution above. Their presence is not a regression; only the
# *absence* of the _pic variants would be.

encapsulated_exe = os.path.join(
    "main_encapsulated", "obj", "main_encapsulated"
)
encapsulated_result = bnr.simple_run([encapsulated_exe])
check(
    "hello from relocated SAL" in encapsulated_result.out,
    "FAIL: relocated encapsulated SAL did not link and run correctly:\n"
    + encapsulated_result.out,
)

# ---------------------------------------------------------------------------
# 3. Install the SAL properly with gprinstall instead of hand-writing an
#    Externally_Built stub. gprinstall generates the correct consuming
#    project file itself (correct Externally_Built/Library_ALI_Dir/interface
#    source layout, etc.) -- removing an entire class of guesswork.
# ---------------------------------------------------------------------------

install_prefix = os.path.abspath("install_orig")

bnr.simple_run(
    [GPRINSTALL, "-p", "--prefix=" + install_prefix,
     os.path.join("tree", "lib.gpr")]
)

generated_project = os.path.join(install_prefix, "share", "gpr", "lib.gpr")

check(
    os.path.isfile(generated_project),
    "FAIL: gprinstall did not produce the expected consuming project at "
    + generated_project,
)

# ---------------------------------------------------------------------------
# 4. End-to-end portability check.
#
#    Physically relocate the whole install prefix -- this is the stand-in
#    for "installed on machine A, consumed on machine B". Then move the
#    original tree/ out of the way too, so nothing can accidentally fall
#    back to it.
# ---------------------------------------------------------------------------

install_relocated = os.path.abspath("install_relocated")
shutil.move(install_prefix, install_relocated)
os.rename("tree", "tree_moved_out_of_the_way")

relocated_share_gpr = os.path.join(install_relocated, "share", "gpr")

# ---------------------------------------------------------------------------
# 5. Build and run the main against the relocated install. main/main.gpr
#    withs a plain "lib.gpr", resolved via GPR_PROJECT_PATH pointing at the
#    relocated install's generated project directory.
# ---------------------------------------------------------------------------

build_env = dict(os.environ)
build_env["GPR_PROJECT_PATH"] = (
    relocated_share_gpr
    + os.pathsep
    + build_env.get("GPR_PROJECT_PATH", "")
)

build_output = bnr.simple_run(
    [GPRBUILD, "-P" + os.path.join("main", "main.gpr"), "-p", "-q", "-f", "-v"],
    env=build_env,
).out

# This is the actual point of this MR: prove Link_Options_Extract really
# resolved the bare -lgnat token back into an absolute path, using THIS
# run's own runtime -- not that linking merely happened to succeed (which
# could hide a silently-wrong resolution if something else papered over
# it, e.g. an implicit -l search finding a coincidentally-compatible
# libgnat.a on the default system search path).
check(
    re.search(r"adalib[/\\]libgnat\.a\b", build_output) is not None,
    "FAIL: final link command shows no resolved libgnat.a -- "
    "Link_Options_Extract did not translate the bare -lgnat token:\n"
    + build_output,
)

main_exe = os.path.join("main", "obj", "main")
result = bnr.simple_run([main_exe])

check(
    "hello from relocated SAL" in result.out,
    "FAIL: relocated SAL did not link and run correctly:\n" + result.out,
)

# ---------------------------------------------------------------------------
# Report
# ---------------------------------------------------------------------------

if FAILURES:
    for msg in FAILURES:
        print(msg)
else:
    print("Direct (non-relocated, non-installed) SAL consumption works")
    print("Embedded linker_options section is portable (bare -lgnat/-lgnarl)")
    print("Tasking fixture's -lgnarl is portable too")
    print("Encapsulated fixture embeds no absolute runtime archive path")
    print("Relocated tasking SAL resolves -lgnat/-lgnarl and runs correctly")
    print("Relocated encapsulated SAL resolves to the _pic runtime and runs")
    print("Relocated, gprinstall-installed SAL linked and ran successfully")