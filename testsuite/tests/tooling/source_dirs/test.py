from testsuite_support.driver.driver_imports import create_scenario
from testsuite_support.driver.driver_imports import add_testcase
from testsuite_support.driver.driver_imports import run
from testsuite_support.driver.driver_constants import ObjOptions as Opt
from testsuite_support.driver.driver_constants import ObjScnCaseValue as Case
from testsuite_support.driver.driver_constants import ObjScnRes as Res
import os

alt_values = {}
create_scenario("Source_Dirs", common_options=Opt.SCN_OPTION_HIDE_PATH_TO_FILE)

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
