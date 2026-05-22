from testsuite_support.driver.driver_imports import create_scenario, add_testcase, run
from testsuite_support.driver.driver_constants import ObjOptions as Opt
from testsuite_support.driver.driver_constants import ObjScnAttrValues as Value
from testsuite_support.driver.driver_constants import ObjScnRes as Res
from testsuite_support.driver.driver_constants import ObjScnCaseValue as Case
from testsuite_support.driver.driver_constants import ObjScnTool as Tool
from testsuite_support.driver.driver_constants import ObjScnPhase as Phase
from testsuite_support.driver.driver_constants import ObjScnAttrSubstPattern as Pattern
from testsuite_support.driver.driver_constants import ObjScnExternalTool as ExtTool
import logging

scn_descr = {
    "attributes": ["Builder.Executable", "Builder.Executable_Suffix"],
    "value_kind": Value.SCN_ATTR_VALUE_SINGLE,
    "setup_cmd": [],
    "test_cmd": [
        {
            "tool": Tool.SCN_TOOL_GPRBUILD,
            "phase": Phase.SCN_PHASE_GPRBUILD_ALL,
            "output_delimiter": f"{ExtTool.SCN_CMD_GCC.value} main.o",
            "expected_behavior": {
                Case.SCN_CASE_VALUE_DEFAULT: {
                    Res.SCN_RES_FOR_ALL: [
                        f"{Pattern.SCN_ATTR_ALT_SUBST_PATTERN_SUFFIX.value}"
                    ]
                },
                Case.SCN_CASE_VALUE_DEF: {
                    Res.SCN_RES_FOR_ALL: [
                        f"{Pattern.SCN_ATTR_ALT_SUBST_PATTERN_SUFFIX.value}"
                    ]
                }
            }
        },
        {
            "tool": Tool.SCN_TOOL_GPRCLEAN,
            "phase": Phase.SCN_PHASE_GPRCLEAN_ALL,
            "output_delimiter": "",
            "expected_behavior": {
                Case.SCN_CASE_VALUE_DEFAULT: {
                    Res.SCN_RES_FOR_ANY: [
                        f"{Pattern.SCN_ATTR_ALT_SUBST_PATTERN_SUFFIX.value}"
                    ]
                },
                Case.SCN_CASE_VALUE_DEF: {
                    Res.SCN_RES_FOR_ANY: [
                        f"{Pattern.SCN_ATTR_ALT_SUBST_PATTERN_SUFFIX.value}"
                    ]
                }
            }
        }
    ],
    "cleanup_cmd": [{"tool": Tool.SCN_TOOL_GPRCLEAN}],
}


alt_values = {}
create_scenario(
    scn_descr,
    common_options=Opt.SCN_OPTION_USE_ALT_ATTR_VALUE,
    log_level=logging.INFO,
)

alt_values[Res.SCN_RES_FOR_ALL] = [("", "main.out", "")]
alt_values[Res.SCN_RES_FOR_ANY] = alt_values[Res.SCN_RES_FOR_ALL]
add_testcase(
    file="files/test_default_name_and_suffix.gpr",
    alt_value=alt_values,
    case_type=Case.SCN_CASE_VALUE_DEFAULT,
)

alt_values[Res.SCN_RES_FOR_ALL] = [("", "foo.out", "")]
alt_values[Res.SCN_RES_FOR_ANY] = alt_values[Res.SCN_RES_FOR_ALL]
add_testcase(
    file="files/test_name_and_suffix.gpr",
    alt_value=alt_values,
    case_type=Case.SCN_CASE_VALUE_DEF,
)

alt_values[Res.SCN_RES_FOR_ALL] = [("", "foo.out", "")]
alt_values[Res.SCN_RES_FOR_ANY] = alt_values[Res.SCN_RES_FOR_ALL]
add_testcase(
    file="files/test_name_suffix_and_same_suffix.gpr",
    alt_value=alt_values,
    case_type=Case.SCN_CASE_VALUE_DEF,
)

alt_values[Res.SCN_RES_FOR_ALL] = [("", "foo.bar.out", "")]
alt_values[Res.SCN_RES_FOR_ANY] = alt_values[Res.SCN_RES_FOR_ALL]
add_testcase(
    file="files/test_name_suffix_and_suffix.gpr",
    alt_value=alt_values,
    case_type=Case.SCN_CASE_VALUE_DEF,
)
run()
