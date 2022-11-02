from testsuite_support.driver.driver_imports import create_scenario, add_testcase, run
from testsuite_support.driver.driver_constants import ObjScnAttrValues as Value
from testsuite_support.driver.driver_constants import ObjScnRes as Res
from testsuite_support.driver.driver_constants import ObjScnCaseValue as Case
from testsuite_support.driver.driver_constants import ObjScnTool as Tool
from testsuite_support.driver.driver_constants import ObjScnPhase as Phase
from testsuite_support.driver.driver_constants import ObjScnAttrSubstPattern as Pattern
from testsuite_support.driver.driver_constants import ObjScnExternalTool as ExtTool
import logging

scn_descr = {
    "tests": ["Builder.Global_Config_File"],
    "value_kind": Value.SCN_ATTR_VALUES_UNIQUE,
    "setup_cmd": [],
    "test_cmd": [
        {
            "tool": Tool.SCN_TOOL_GPRBUILD,
            "phase": Phase.SCN_PHASE_GPRBUILD_COMPILATION,
            "output_delimiter": f"{ExtTool.SCN_CMD_GCC.value} -c",
            "expected_behavior": {
                Case.SCN_CASE_VALUE_DEF: {
                    Res.SCN_RES_FOR_ALL: [
                        "-gnatA",
                        f"-gnatec={Pattern.SCN_ATTR_SUBSTITUTE_PATTERN.value}",
                    ]
                },
                Case.SCN_CASE_VALUE_UNDEF: {
                    Res.SCN_RES_FOR_ALL: ["-gnatA"]
                },
                Case.SCN_CASE_VALUE_INVALID: {
                    Res.SCN_RES_FOR_ALL: [
                        "-gnatA",
                        f"-gnatec={Pattern.SCN_ATTR_SUBSTITUTE_PATTERN.value}",
                    ]
                }
            }
        }
    ],
    "cleanup_cmd": [{"tool": Tool.SCN_TOOL_GPRCLEAN}],
}


create_scenario(scn_descr, log_level=logging.INFO)
add_testcase(file="files/test_config.gpr", case_type=Case.SCN_CASE_VALUE_DEF)
add_testcase(file="files/test_no_config.gpr", case_type=Case.SCN_CASE_VALUE_UNDEF)
add_testcase(
    file="files/test_unknown_config.gpr", case_type=Case.SCN_CASE_VALUE_INVALID
)
run()
