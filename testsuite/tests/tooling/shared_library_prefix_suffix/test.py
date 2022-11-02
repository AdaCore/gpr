from e3.env import Env
from testsuite_support.driver.driver_imports import create_scenario, add_testcase, run
from testsuite_support.driver.driver_imports import edit_custom_command
from testsuite_support.driver.driver_constants import ObjOptions as Opt
from testsuite_support.driver.driver_constants import ObjScnAttrValues as Value
from testsuite_support.driver.driver_constants import ObjScnRes as Res
from testsuite_support.driver.driver_constants import ObjScnCaseValue as Case
from testsuite_support.driver.driver_constants import ObjScnTool as Tool
from testsuite_support.driver.driver_constants import ObjScnPhase as Phase
from testsuite_support.driver.driver_constants import ObjScnAttrSubstPattern as Pattern
from testsuite_support.driver.driver_constants import ObjScnExternalTool as ExtTool
import logging
import re

scn_descr = {
    "attributes": [
        "Project_Level.Shared_Library_Prefix",
        "Project_Level.Shared_Library_Suffix",
    ],
    "value_kind": Value.SCN_ATTR_VALUE_SINGLE,
    "setup_cmd": [
        {"tool": Tool.SCN_TOOL_GPRCONFIG},
        {"tool": Tool.SCN_TOOL_CUSTOM, "phase": Phase.SCN_PHASE_CUSTOM_SLOT_1},
        {"tool": Tool.SCN_TOOL_GPRBUILD, "phase": Phase.SCN_PHASE_GPRBUILD_COMPILATION},
    ],
    "test_cmd": [
        {
            "tool": Tool.SCN_TOOL_GPRBUILD,
            "phase": Phase.SCN_PHASE_GPRBUILD_BIND,
            "output_delimiter": f"{ExtTool.SCN_CMD_GCC.value} -shared",
            "expected_behavior": {
                Case.SCN_CASE_VALUE_DEF: {
                    Res.SCN_RES_FOR_ANY: [
                        f"-o {Pattern.SCN_ATTR_ALT_SUBST_PATTERN_ALL.value}"
                    ]
                },
                Case.SCN_CASE_VALUE_DEFAULT: {
                    Res.SCN_RES_FOR_ANY: [
                        f"-o {Pattern.SCN_ATTR_ALT_SUBST_PATTERN_ALL.value}"
                    ]
                }
            }
        },
        {
            "tool": Tool.SCN_TOOL_GPRBUILD,
            "phase": Phase.SCN_PHASE_GPRBUILD_LINK,
            "output_delimiter": f"{ExtTool.SCN_CMD_GCC.value} main.o",
            "expected_behavior": {
                Case.SCN_CASE_VALUE_DEF: {
                    Res.SCN_RES_FOR_NONE: [
                        f"-l{Pattern.SCN_ATTR_ALT_SUBST_PATTERN_NAME.value}"
                    ]
                },
                Case.SCN_CASE_VALUE_DEFAULT: {
                    Res.SCN_RES_FOR_ANY: [
                        f"-l{Pattern.SCN_ATTR_ALT_SUBST_PATTERN_NAME.value}"
                    ]
                }
            }
        }
    ],
    "cleanup_cmd": [{"tool": Tool.SCN_TOOL_GPRCLEAN}],
}


if Env().host.platform.endswith("windows"):
    default_suffix = ".dll"
else:
    default_suffix = ".so"


def custom_cmd():
    replaced_content = ""
    with open("files/alternative.cgpr", "r") as fp:
        for line in fp:
            new_line = line.strip("\n")
            if "Shared_Library_Prefix" in line:
                new_line = re.sub('"(.*)"', '"some_prefix_"', line.strip())
            elif "Shared_Library_Suffix" in line:
                new_line = re.sub('"(.*)"', '".some_suffix"', line.strip())
            replaced_content = replaced_content + new_line + "\n"
    with open("files/alternative.cgpr", "w") as fp:
        fp.write(replaced_content)


alt_values = {}
create_scenario(
    scn_descr, common_options=Opt.SCN_OPTION_USE_ALT_ATTR_VALUE,
    log_level=logging.INFO
)
edit_custom_command(custom_cmd=custom_cmd)

alt_values[Res.SCN_RES_FOR_ANY] = [("lib", "lib1", default_suffix)]
add_testcase(
    file="files/test.gpr", case_type=Case.SCN_CASE_VALUE_DEFAULT, alt_value=alt_values,
    options=Opt.SCN_OPTION_HIDE_EXPECTED_RES
)

alt_values[Res.SCN_RES_FOR_NONE] = [("", "lib1", "")]
alt_values[Res.SCN_RES_FOR_ANY] = [("some_prefix_", "lib1", ".some_suffix")]
add_testcase(
    file="files/test.gpr",
    case_type=Case.SCN_CASE_VALUE_DEF,
    alt_value=alt_values,
    options=Opt.SCN_OPTION_USE_CGPR
)

alt_values[Res.SCN_RES_FOR_NONE] = [("", "lib2", "")]
alt_values[Res.SCN_RES_FOR_ANY] = [("some_other_prefix_", "lib2", ".some_other_suffix")]
add_testcase(
    file="files/test2.gpr", case_type=Case.SCN_CASE_VALUE_DEF, alt_value=alt_values
)

add_testcase(
    file="files/test2.gpr",
    case_type=Case.SCN_CASE_VALUE_DEF,
    alt_value=alt_values,
    options=Opt.SCN_OPTION_USE_CGPR
)
run()
