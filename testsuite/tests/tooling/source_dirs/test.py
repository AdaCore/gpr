from testsuite_support.driver.driver_imports import create_scenario, add_testcase, run
from testsuite_support.driver.driver_constants import ObjOptions as Opt
from testsuite_support.driver.driver_constants import ObjScnAttrValues as Value
from testsuite_support.driver.driver_constants import ObjScnRes as Res
from testsuite_support.driver.driver_constants import ObjScnCaseValue as Case
from testsuite_support.driver.driver_constants import ObjScnTool as Tool
from testsuite_support.driver.driver_constants import ObjScnPhase as Phase
from testsuite_support.driver.driver_constants import ObjScnAttrSubstPattern as Pattern
import logging
import os

scn_descr = {
    "tests": ["Project_Level.Source_Dirs"],
    "value_kind": Value.SCN_ATTR_VALUES_DISTRIBUTED,
    "setup_cmd": [],
    "test_cmd": [
        {
            "tool": Tool.SCN_TOOL_GPRLS,
            "phase": Phase.SCN_PHASE_GPRLS_SOURCE_PARSING,
            "output_delimiter": "",
            "expected_behavior": {
                Case.SCN_CASE_VALUE_UNDEF: {
                    Res.SCN_RES_FOR_EMPTY: []
                },
                Case.SCN_CASE_VALUE_DEF: {
                    Res.SCN_RES_FOR_NONE: [
                        f"{Pattern.SCN_ATTR_ALT_SUBST_PATTERN_PREFIX.value}"
                    ],
                    Res.SCN_RES_FOR_ANY: [
                        f"{Pattern.SCN_ATTR_ALT_SUBST_PATTERN_PREFIX.value}"
                    ]
                },
                Case.SCN_CASE_VALUE_INVALID: {
                    Res.SCN_RES_FOR_ANY: [
                        "is not a valid directory",
                        "gprls: unable to process project file",
                    ]
                }
            }
        }
    ],
    "cleanup_cmd": [],
}

alt_values = {}
create_scenario(scn_descr, common_options=Opt.SCN_OPTION_HIDE_PATH_TO_FILE,
                log_level=logging.INFO)

add_testcase(file="files/test_no_src_dirs.gpr", case_type=Case.SCN_CASE_VALUE_UNDEF)

alt_values[Res.SCN_RES_FOR_ANY] = [
    (os.getcwd(), "/files/src1", ""),
    (os.getcwd(), "/files/src1/src1", ""),
    (os.getcwd(), "/files/src1/src2", ""),
    (os.getcwd(), "/files/src2", ""),
    (os.getcwd(), "/files/src3", ""),
]
add_testcase(
    file="files/test_src_dirs.gpr",
    case_type=Case.SCN_CASE_VALUE_DEF,
    alt_value=alt_values,
    options=Opt.SCN_OPTION_USE_ALT_ATTR_VALUE
    | Opt.SCN_OPTION_CONVERT_FILE_TO_DIRNAME
    | Opt.SCN_OPTION_FULL_STRING_COMPARISION,
)

alt_values[Res.SCN_RES_FOR_NONE] = [
    (os.getcwd(), "/files/src1/src1", ""),
    (os.getcwd(), "/files/src1/src2", ""),
]
alt_values[Res.SCN_RES_FOR_ANY] = [
    (os.getcwd(), "/files/src1", ""),
    (os.getcwd(), "/files/src2", ""),
    (os.getcwd(), "/files/src3", ""),
]
add_testcase(
    file="files/test_src_dirs_excluded_dirs.gpr",
    case_type=Case.SCN_CASE_VALUE_DEF,
    alt_value=alt_values,
    options=Opt.SCN_OPTION_USE_ALT_ATTR_VALUE
    | Opt.SCN_OPTION_CONVERT_FILE_TO_DIRNAME
    | Opt.SCN_OPTION_FULL_STRING_COMPARISION,
)
add_testcase(
    file="files/test_src_dirs_ignore_source_sub_dirs.gpr",
    case_type=Case.SCN_CASE_VALUE_DEF,
    alt_value=alt_values,
    options=Opt.SCN_OPTION_USE_ALT_ATTR_VALUE
    | Opt.SCN_OPTION_CONVERT_FILE_TO_DIRNAME
    | Opt.SCN_OPTION_FULL_STRING_COMPARISION,
)
add_testcase(
    file="files/test_src_dirs_excluded_and_ignore.gpr",
    case_type=Case.SCN_CASE_VALUE_DEF,
    alt_value=alt_values,
    options=Opt.SCN_OPTION_USE_ALT_ATTR_VALUE
    | Opt.SCN_OPTION_CONVERT_FILE_TO_DIRNAME
    | Opt.SCN_OPTION_FULL_STRING_COMPARISION,
)

alt_values[Res.SCN_RES_FOR_NONE] = [(os.getcwd(), "/files/src1", "")]
alt_values[Res.SCN_RES_FOR_ANY] = [
    (os.getcwd(), "/files/src2", ""),
    (os.getcwd(), "/files/src3", ""),
    (os.getcwd(), "/files/src1/src1", ""),
    (os.getcwd(), "/files/src1/src2", ""),
]
add_testcase(
    file="files/test_src_dirs_excluded_dirs_2.gpr",
    case_type=Case.SCN_CASE_VALUE_DEF,
    alt_value=alt_values,
    options=Opt.SCN_OPTION_USE_ALT_ATTR_VALUE
    | Opt.SCN_OPTION_CONVERT_FILE_TO_DIRNAME
    | Opt.SCN_OPTION_FULL_STRING_COMPARISION,
)

add_testcase(
    file="files/test_invalid_src_dirs.gpr",
    case_type=Case.SCN_CASE_VALUE_INVALID,
    options=Opt.SCN_OPTION_RES_AS_A_FULL_STRING,
)

run()
