from testsuite_support.driver.driver_constants import ObjScnAttrValues as oav
from testsuite_support.driver.driver_constants import ObjScnRes as osr
from testsuite_support.driver.driver_constants import ObjScnCaseValue as ocv
from testsuite_support.driver.driver_constants import ObjScnTool as ost
from testsuite_support.driver.driver_constants import ObjScnPhase as osp
from testsuite_support.driver.driver_constants import ObjScnAttrSubstPattern as osa


""" This structure describes every test that can be ran.
    Each attributes has its set of setup and cleanup command, the direct command which
    output is monitored to a have a specific set behavior.
    "value_kind" : Is the attribute value a simple value or a list. If it is a list
                   are the values meant to be concatenated or distributed.
    "setup_cmd": common command to every testcase that will be run before the main
                 command. This set of command will not be monitored and will not
                 impact the testcase result (except if the result is directly dependent
                 on it).
                 Custom command can be defined if they are not (itself or a close
                 variant) present in driver_constants.py. Especially useful to make
                 small modification between two GPRtools command (modifying an
                 attribute value in a previously created file by a setup command)
    "cleanup_cmd": common command to every testcase that will be run after de main
                   command. This set of command will not be monitored and will not
                 impact the testcase result (except if the result is directly dependent
                 on it).
                 Custom command can be defined if they are not (itself or a close
                 variant) present in driver_constants.py. Especially useful to make
                 small modification between two cleanup command (deleting files that
                 could influence the next testcase)
    "test_cmd": Mains command of the test. Those command are being monitored and will
                determine the result of the test.
        "tool": The tool that will be launched by the driver.
        "phase": Which phase of the tool will be launched. If it's a subcommand of a
                 toolchain, the prior individual commands must be set in the "setup_cmd"
                 Eg : A test want to monitor GPRbuild Linking phase. Compilation and
                    Binding commands must be launched as a setup prior to the link.
        "output_delimiter": String sequence that will be used to select a set of lines
                            from main command output to monitor the result in.
        "expected_behavior": Definition of the results to look for. Expected behavior
                             can differ based on if the attribute is defined, undefined,
                             set as default of set to an invalid value.
                             For each of those cases it is possible to define what it is
                             expected to happen.
                             Based on the attribute specificity and its value, you can
                             expect to find the behavior on every lines of the output,
                             at least one time, or not at all.
"""
SCN_ATTRIBUTE_TEST_CONFIG = {
    "Builder.Global_Config_File": {
        "value_kind": oav.SCN_ATTR_VALUES_UNIQUE,
        "setup_cmd": [],
        "test_cmd": [
            {
                "tool": ost.SCN_TOOL_GPRBUILD,
                "phase": osp.SCN_PHASE_GPRBUILD_COMPILATION,
                "output_delimiter": "gcc -c",
                "expected_behavior": {
                    ocv.SCN_CASE_VALUE_DEF: {
                        osr.SCN_RES_FOR_ALL:
                            ["-gnatA",
                             f"-gnatec={osa.SCN_ATTR_SUBSTITUTE_PATTERN.value}"]
                    },
                    ocv.SCN_CASE_VALUE_UNDEF: {
                        osr.SCN_RES_FOR_ALL: ["-gnatA"]

                    },
                    ocv.SCN_CASE_VALUE_INVALID: {
                        osr.SCN_RES_FOR_ALL:
                            ["-gnatA",
                             f"-gnatec={osa.SCN_ATTR_SUBSTITUTE_PATTERN.value}"]
                    }
                }
            }
        ],
        "cleanup_cmd": [
            {"tool": ost.SCN_TOOL_GPRCLEAN}
        ]
    },
    "Project_Level.Shared_Library_Prefix": {
        "value_kind": oav.SCN_ATTR_VALUES_UNIQUE,
        "setup_cmd": [
            {"tool": ost.SCN_TOOL_GPRCONFIG},
            {"tool": ost.SCN_TOOL_CUSTOM, "phase": osp.SCN_PHASE_CUSTOM_SLOT_1},
            {"tool": ost.SCN_TOOL_GPRBUILD, "phase": osp.SCN_PHASE_GPRBUILD_COMPILATION}
        ],
        "test_cmd": [
            {
                "tool": ost.SCN_TOOL_GPRBUILD, "phase": osp.SCN_PHASE_GPRBUILD_BIND,
                "output_delimiter": "gcc -shared",
                "expected_behavior": {
                    ocv.SCN_CASE_VALUE_DEF: {
                        osr.SCN_RES_FOR_ANY:
                            [f"-o {osa.SCN_ATTR_ALT_SUBST_PATTERN_ALL.value}"],
                    },
                    ocv.SCN_CASE_VALUE_DEFAULT: {
                        osr.SCN_RES_FOR_ANY:
                            [f"-o {osa.SCN_ATTR_ALT_SUBST_PATTERN_ALL.value}"],
                    },
                }
            },
            {
                "tool": ost.SCN_TOOL_GPRBUILD, "phase": osp.SCN_PHASE_GPRBUILD_LINK,
                "output_delimiter": "gcc main.o",
                "expected_behavior": {
                    ocv.SCN_CASE_VALUE_DEF: {
                        osr.SCN_RES_FOR_NONE:
                            [f"-l{osa.SCN_ATTR_ALT_SUBST_PATTERN_NAME.value}"],
                    },
                    ocv.SCN_CASE_VALUE_DEFAULT: {
                        osr.SCN_RES_FOR_ANY:
                            [f"-l{osa.SCN_ATTR_ALT_SUBST_PATTERN_NAME.value}"],

                    }
                }
            }
        ],
        "cleanup_cmd": [
            {"tool": ost.SCN_TOOL_GPRCLEAN}
        ]
    }
}
