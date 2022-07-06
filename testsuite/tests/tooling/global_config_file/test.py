from testsuite_support.driver.driver_imports import create_scenario, add_testcase, run
from testsuite_support.driver.driver_constants import ObjScnCaseValue as Case


create_scenario("Builder.Global_Config_File")
add_testcase(file="files/test_config.gpr", case_type=Case.SCN_CASE_VALUE_DEF)
add_testcase(file="files/test_no_config.gpr", case_type=Case.SCN_CASE_VALUE_UNDEF)
add_testcase(
    file="files/test_unknown_config.gpr", case_type=Case.SCN_CASE_VALUE_INVALID
)
run()
