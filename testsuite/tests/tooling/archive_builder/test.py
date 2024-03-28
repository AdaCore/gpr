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
    "attributes": ["Project_Level.Archive_Builder"],

    "value_kind": Value.SCN_ATTR_VALUE_SINGLE,
    "setup_cmd": [],
    "test_cmd": [
        {
            "tool": Tool.SCN_TOOL_GPRCLEAN,
            "phase": Phase.SCN_PHASE_GPRCLEAN_ALL,
            "output_delimiter": "",
            "expected_behavior": {
                Case.SCN_CASE_VALUE_EMPTY: {
                    Res.SCN_RES_FOR_ANY: [
                        "empty Archive_builder is not supported yet.",
                    ]
                }
            }
        },
        {
            "tool": Tool.SCN_TOOL_GPRINSTALL,
            "phase": Phase.SCN_PHASE_GPRINSTALL_ALL,
            "output_delimiter": "",
            "expected_behavior": {
                Case.SCN_CASE_VALUE_EMPTY: {
                    Res.SCN_RES_FOR_ANY: [
                        "empty Archive_builder is not supported yet.",
                    ]
                }
            }
        }
    ],
    "cleanup_cmd": []
}

alt_values = {}
create_scenario(scn_descr, common_options=Opt.SCN_OPTION_RES_AS_A_FULL_STRING,
                log_level=logging.INFO)

add_testcase(file="prj.gpr", case_type=Case.SCN_CASE_VALUE_EMPTY)

run()
