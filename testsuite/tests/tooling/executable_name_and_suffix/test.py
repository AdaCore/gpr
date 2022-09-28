from testsuite_support.driver.driver_imports import create_scenario, add_testcase, run
from testsuite_support.driver.driver_constants import ObjOptions as Opt
from testsuite_support.driver.driver_constants import ObjScnCaseValue as Case
from testsuite_support.driver.driver_constants import ObjScnRes as Res
import logging

alt_values = {}
create_scenario("Builder.Executable", common_options=Opt.SCN_OPTION_USE_ALT_ATTR_VALUE,
                log_level=logging.INFO)

alt_values[Res.SCN_RES_FOR_ALL] = [("", "main.out", "")]
alt_values[Res.SCN_RES_FOR_ANY] = alt_values[Res.SCN_RES_FOR_ALL]
add_testcase(file="files/test_default_name_and_suffix.gpr",
             alt_value=alt_values,
             case_type=Case.SCN_CASE_VALUE_DEF)

alt_values[Res.SCN_RES_FOR_ALL] = [("", "foo.out", "")]
alt_values[Res.SCN_RES_FOR_ANY] = alt_values[Res.SCN_RES_FOR_ALL]
add_testcase(file="files/test_name_and_suffix.gpr",
             alt_value=alt_values,
             case_type=Case.SCN_CASE_VALUE_DEF)

alt_values[Res.SCN_RES_FOR_ALL] = [("", "foo.out", "")]
alt_values[Res.SCN_RES_FOR_ANY] = alt_values[Res.SCN_RES_FOR_ALL]
add_testcase(file="files/test_name_suffix_and_same_suffix.gpr",
             alt_value=alt_values,
             case_type=Case.SCN_CASE_VALUE_DEF)

alt_values[Res.SCN_RES_FOR_ALL] = [("", "foo.bar.out", "")]
alt_values[Res.SCN_RES_FOR_ANY] = alt_values[Res.SCN_RES_FOR_ALL]
add_testcase(file="files/test_name_suffix_and_suffix.gpr",
             alt_value=alt_values,
             case_type=Case.SCN_CASE_VALUE_DEF)
run()
