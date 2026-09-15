"""Each stage prints every action's status: SKIPPED means it did not run."""

import json
import os
import shutil

from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD

bnr = BuilderAndRunner()
cwd = os.getcwd()
stage = 0
edits = 0


def use(variant):
    """Make VARIANT the project file.

    All variants declare project Proj, so the view and object directory stay
    the same and the previous build's signature is read back.
    """
    shutil.copyfile(variant, "proj.gpr")


def build(*args, expect):
    """Build proj.gpr with ARGS and report each action's status."""
    global stage
    stage += 1
    print(f"---- {stage}: {expect}")

    bnr.call([GPRBUILD, "-p", "-q", "-j1", "--json-summary", *args,
              "-P", "proj.gpr"])

    with open("jobs.json") as fp:
        jobs = json.load(fp)

    statuses = {job["uid"]: job["status"] for job in jobs}
    for uid in sorted(statuses):
        print(f"   {uid}: {statuses[uid]}")


def gnatec(adc):
    return f"-gnatec={os.path.join(cwd, adc)}"


def edit(adc):
    """Rewrite ADC in place, under the same name.

    The pragmas are left alone and only a comment varies, which is enough to
    change the file's checksum while keeping every stage compilable. Nothing
    on the command line differs, so a rebuild can only come from the file
    artifact.
    """
    global edits
    edits += 1

    with open(adc, "w") as fp:
        fp.write(f"--  edit {edits}\n")
        fp.write("pragma Restrictions (No_Abort_Statements);\n")


#  No configuration

use("proj-no-conf.gpr")
build(expect="initial build, no configuration file")
build(expect="nothing changed, everything skipped")

#  Command line

build(gnatec("pragmas.adc"), expect="-gnatec= added on the command line")
build(gnatec("pragmas.adc"), expect="same command line, everything skipped")

edit("pragmas.adc")
build(gnatec("pragmas.adc"), expect="pragmas.adc edited, same command line")
build(gnatec("pragmas.adc"), expect="nothing changed, everything skipped")

build(gnatec("pragmas2.adc"), expect="-gnatec= now names another file")
build(expect="-gnatec= removed from the command line")
build(expect="ADC gone from the ALI too, everything skipped")

#  Configuration pragmas attributes

use("proj_global_pragmas.gpr")
build(expect="Builder.Global_Configuration_Pragmas added")
build(expect="attributes unchanged, everything skipped")

edit("pragmas.adc")
build(expect="pragmas.adc edited, attribute unchanged")
build(expect="nothing changed, everything skipped")

use("proj_local_pragmas.gpr")
build(expect="Compiler.Local_Configuration_Pragmas added on top")

use("proj-no-conf.gpr")
build(expect="both configuration pragmas attributes removed")
build(expect="ADCs gone from the ALI too, everything skipped")

#  Configuration file attributes

use("proj_global_config_file.gpr")
build(expect="Builder.Global_Config_File (Ada) added")
build(expect="attributes unchanged, everything skipped")

edit("pragmas.adc")
build(expect="pragmas.adc edited, Global_Config_File unchanged")
build(expect="nothing changed, everything skipped")

use("proj_local_config_file.gpr")
build(expect="Compiler.Local_Config_File (Ada) added on top")
build(expect="attributes unchanged, everything skipped")

edit("pragmas2.adc")
build(expect="pragmas2.adc edited, Local_Config_File unchanged")
build(expect="nothing changed, everything skipped")

use("proj-no-conf.gpr")
build(expect="both configuration file attributes removed")
build(expect="ADCs gone from the ALI too, everything skipped")
