from testsuite_support.driver.driver_imports import create_scenario, add_testcase, run
from testsuite_support.driver.driver_imports import edit_custom_command
from testsuite_support.driver.driver_constants import ObjOptions as opt
from testsuite_support.driver.driver_constants import ObjScnCaseValue as ocv
import re
import logging


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


create_scenario("Shared_Library_Prefix", log_level=logging.INFO)
edit_custom_command(custom_cmd=custom_cmd)
add_testcase(file="files/test.gpr", type=ocv.SCN_CASE_VALUE_DEFAULT,
             altvalue=[("lib", "lib1", ".so")],
             options=opt.SCN_OPTION_USE_ALT_ATTR_VALUE)
add_testcase(file="files/test.gpr", type=ocv.SCN_CASE_VALUE_DEF,
             altvalue=[("some_prefix_", "lib1", ".some_suffix")],
             options=opt.SCN_OPTION_USE_CGPR | opt.SCN_OPTION_USE_ALT_ATTR_VALUE)
add_testcase(file="files/test2.gpr", type=ocv.SCN_CASE_VALUE_DEF,
             altvalue=[("some_other_prefix_", "lib2", ".some_other_suffix")],
             options=opt.SCN_OPTION_USE_ALT_ATTR_VALUE)
add_testcase(file="files/test2.gpr", type=ocv.SCN_CASE_VALUE_DEF,
             altvalue=[("some_other_prefix_", "lib2", ".some_other_suffix")],
             options=opt.SCN_OPTION_USE_CGPR | opt.SCN_OPTION_USE_ALT_ATTR_VALUE)
run()
