from enum import IntFlag, Enum
from e3.env import Env


class ObjScnTool(Enum):
    SCN_TOOL_NONE = 0
    SCN_TOOL_GPRBUILD = 1
    SCN_TOOL_GPRCLEAN = 2
    SCN_TOOL_GPRDOC = 3
    SCN_TOOL_GPRCONFIG = 4
    SCN_TOOL_GPRLS = 5
    SCN_TOOL_CUSTOM = 99

    def tool_name(self):
        return self.name.lower().split("_")[-1]

    def doc(self):
        to_print = f"- {self.name}"
        if self is ObjScnTool.SCN_TOOL_NONE:
            return f"{to_print} : Default value. Nothing will be executed."
        if self is ObjScnTool.SCN_TOOL_GPRBUILD:
            return f"{to_print} : GPRBuild will be executed."
        if self is ObjScnTool.SCN_TOOL_GPRCLEAN:
            return f"{to_print} : GPRClean will be executed."
        if self is ObjScnTool.SCN_TOOL_GPRDOC:
            return f"{to_print} : GPRDoc will be executed."
        if self is ObjScnTool.SCN_TOOL_GPRCONFIG:
            return f"{to_print} : GPRConfig will be executed."
        if self is ObjScnTool.SCN_TOOL_GPRLS:
            return f"{to_print} : GPRls will be executed."
        if self is ObjScnTool.SCN_TOOL_CUSTOM:
            return f"{to_print} : Test custom command will be executed. Sometime a " \
                   + "test requires something that cannot be done with GPRTools "\
                   + "(E.g.: Modifying an attribute value in a config file). For " \
                   + "such cases custom tool exists. There are 8 slots (phases) " \
                   + "available [SCN_PHASE_CUSTOM_SLOT_X]. Each slot can be filled by" \
                   + "calling driver_imports.edit_custom_command from your test file."


class ObjScnPhase(Enum):
    SCN_PHASE_NONE = 0
    SCN_PHASE_GPRBUILD_COMPILATION = 1
    SCN_PHASE_GPRBUILD_BIND = 2
    SCN_PHASE_GPRBUILD_LINK = 3
    SCN_PHASE_GPRBUILD_ALL = 4
    SCN_PHASE_GPRCLEAN_ALL = 5
    SCN_PHASE_GPRLS_SOURCE_PARSING = 6
    SCN_PHASE_CUSTOM_SLOT_1 = 101
    SCN_PHASE_CUSTOM_SLOT_2 = 102
    SCN_PHASE_CUSTOM_SLOT_3 = 103
    SCN_PHASE_CUSTOM_SLOT_4 = 104
    SCN_PHASE_CUSTOM_SLOT_5 = 105
    SCN_PHASE_CUSTOM_SLOT_6 = 106
    SCN_PHASE_CUSTOM_SLOT_7 = 107
    SCN_PHASE_CUSTOM_SLOT_8 = 108

    def doc(self):
        to_print = f"- {self.name}"
        if self is ObjScnPhase.SCN_PHASE_NONE:
            return f"{to_print} : Only the generic Tool command will be executed"
        if self is ObjScnPhase.SCN_PHASE_GPRBUILD_COMPILATION:
            return f"{to_print} : Only works when executing Tool.SCN_TOOL_GPRBUILD. " \
                   + "Only the compilation part of GPRBuild will be executed."
        if self is ObjScnPhase.SCN_PHASE_GPRBUILD_BIND:
            return f"{to_print} : Only works when executing Tool.SCN_TOOL_GPRBUILD. "\
                   + "Only the bind part of GPRBuild will be executed."
        if self is ObjScnPhase.SCN_PHASE_GPRBUILD_LINK:
            return f"{to_print} : Only works when executing Tool.SCN_TOOL_GPRBUILD. "\
                   + "Only the link part of GPRBuild will be executed."
        if self is ObjScnPhase.SCN_PHASE_GPRBUILD_ALL:
            return f"{to_print} : Only works when executing Tool.SCN_TOOL_GPRBUILD. "\
                   + "Do the whole process."
        if self is ObjScnPhase.SCN_PHASE_GPRCLEAN_ALL:
            return f"{to_print} : Only works when executing Tool.SCN_TOOL_GPRCLEAN. "\
                   + " Do the whole process. Only shows what will be removed."
        if self is ObjScnPhase.SCN_PHASE_GPRLS_SOURCE_PARSING:
            return f"{to_print} : Only works when executing Tool.SCN_TOOL_GPRLS. "\
                   + "List the sources."
        if self is ObjScnPhase.SCN_PHASE_CUSTOM_SLOT_1:
            return f"{to_print} : Only works when using Tool.SCN_TOOL_CUSTOM. This " \
                   + " slot can be filled by calling " \
                   + "driver_imports.edit_custom_command from your test file."
        if self is ObjScnPhase.SCN_PHASE_CUSTOM_SLOT_2:
            return f"{to_print} : Only works when using Tool.SCN_TOOL_CUSTOM. This " \
                   + " slot can be filled by calling " \
                   + "driver_imports.edit_custom_command from your test file."
        if self is ObjScnPhase.SCN_PHASE_CUSTOM_SLOT_3:
            return f"{to_print} : Only works when using Tool.SCN_TOOL_CUSTOM. This " \
                   + " slot can be filled by calling " \
                   + "driver_imports.edit_custom_command from your test file."
        if self is ObjScnPhase.SCN_PHASE_CUSTOM_SLOT_4:
            return f"{to_print} : Only works when using Tool.SCN_TOOL_CUSTOM. This " \
                   + " slot can be filled by calling " \
                   + "driver_imports.edit_custom_command from your test file."
        if self is ObjScnPhase.SCN_PHASE_CUSTOM_SLOT_5:
            return f"{to_print} : Only works when using Tool.SCN_TOOL_CUSTOM. This " \
                   + " slot can be filled by calling " \
                   + "driver_imports.edit_custom_command from your test file."
        if self is ObjScnPhase.SCN_PHASE_CUSTOM_SLOT_6:
            return f"{to_print} : Only works when using Tool.SCN_TOOL_CUSTOM. This " \
                   + " slot can be filled by calling " \
                   + "driver_imports.edit_custom_command from your test file."
        if self is ObjScnPhase.SCN_PHASE_CUSTOM_SLOT_7:
            return f"{to_print} : Only works when using Tool.SCN_TOOL_CUSTOM. This " \
                   + " slot can be filled by calling " \
                   + "driver_imports.edit_custom_command from your test file."
        if self is ObjScnPhase.SCN_PHASE_CUSTOM_SLOT_8:
            return f"{to_print} : Only works when using Tool.SCN_TOOL_CUSTOM. This " \
                   + " slot can be filled by calling " \
                   + "driver_imports.edit_custom_command from your test file."


class ObjScnExternalTool(Enum):
    if Env().host.platform.endswith('windows'):
        SCN_CMD_GCC = "gcc.exe'"
    else:
        SCN_CMD_GCC = "gcc"


class ObjScnOutput(Enum):
    SCN_OUTPUT_UNDEFINED = 0
    SCN_OUTPUT_FILE_LIST = 1
    SCN_OUTPUT_CMD_LINE = 2

    def doc(self):
        to_print = f"- {self.name}"
        if self is ObjScnOutput.SCN_OUTPUT_UNDEFINED:
            return f"{to_print} : Default value. Should not be used"
        if self is ObjScnOutput.SCN_OUTPUT_FILE_LIST:
            return f"{to_print} : The output is a list of files "\
                   + "(E.g. : GPRLs source parser)"
        if self is ObjScnOutput.SCN_OUTPUT_CMD_LINE:
            return f"{to_print} : The output is a standard tool output "\
                   + "(E.g. : GPRBuild)."


class ObjScnCaseValue(Enum):
    SCN_CASE_VALUE_DEFAULT = 0
    SCN_CASE_VALUE_UNDEF = 1
    SCN_CASE_VALUE_DEF = 2
    SCN_CASE_VALUE_INVALID = 4

    def doc(self):
        to_print = f"- {self.name}"
        if self is ObjScnCaseValue.SCN_CASE_VALUE_DEFAULT:
            return f"{to_print} : This describes the case when the attribute is "\
                   + "undefined and a default value is available."
        if self is ObjScnCaseValue.SCN_CASE_VALUE_UNDEF:
            return f"{to_print} : This describes the case when the attribute is "\
                   + "undefined and no default value is available."
        if self is ObjScnCaseValue.SCN_CASE_VALUE_DEF:
            return f"{to_print} : This describes the case when the attribute is "\
                   + "defined with a valid value."
        if self is ObjScnCaseValue.SCN_CASE_VALUE_INVALID:
            return f"{to_print} : This describes the case when the attribute is "\
                   + "defined with an invalid value."


class ObjScnRes(Enum):
    SCN_RES_FOR_EMPTY = 0
    SCN_RES_FOR_NONE = 1
    SCN_RES_FOR_ANY = 2
    SCN_RES_FOR_ALL = 3

    def empty(self):
        if self is ObjScnRes.SCN_RES_FOR_EMPTY:
            return True
        return False

    def none(self):
        if self is ObjScnRes.SCN_RES_FOR_NONE:
            return True
        return False

    def any(self):
        if self is ObjScnRes.SCN_RES_FOR_ANY:
            return True
        return False

    def all(self):
        if self is ObjScnRes.SCN_RES_FOR_ALL:
            return True
        return False

    def doc(self):
        to_print = f"- {self.name}"
        if self is ObjScnRes.SCN_RES_FOR_EMPTY:
            return f"{to_print} : Your expected output is to be empty."
        if self is ObjScnRes.SCN_RES_FOR_NONE:
            return f"{to_print} : The value is expected not to be found in your " \
                   + "output. Particularly useful when checking the absence of a "\
                   + "source file or a switch."
        if self is ObjScnRes.SCN_RES_FOR_ANY:
            return f"{to_print} : The value is expected to be found at least once in "\
                   + "your output. Particularly useful to check the presence of a "\
                   + "source file in a list of files."
        if self is ObjScnRes.SCN_RES_FOR_ALL:
            return f"{to_print} : The value is expected to be found on each line of " \
                   + "your output. Particularly useful to check the presence of a " \
                   + "compilation switch."


class ObjScnAttrValues(Enum):
    SCN_ATTR_VALUES_UNKNOWN = 0
    SCN_ATTR_VALUE_SINGLE = 1
    SCN_ATTR_VALUE_LIST_CONCATENATED = 2
    SCN_ATTR_VALUE_LIST_DISTRIBUTED = 3

    def unique(self):
        if self is ObjScnAttrValues.SCN_ATTR_VALUE_SINGLE:
            return True
        return False

    def concatenated(self):
        if self is ObjScnAttrValues.SCN_ATTR_VALUE_LIST_CONCATENATED:
            return True
        return False

    def distributed(self):
        if self is ObjScnAttrValues.SCN_ATTR_VALUE_LIST_DISTRIBUTED:
            return True
        return False

    def doc(self):
        to_print = f"- {self.name}"
        if self is ObjScnAttrValues.SCN_ATTR_VALUES_UNKNOWN:
            return f"{to_print} : Default value. Should not be used."
        if self is ObjScnAttrValues.SCN_ATTR_VALUE_SINGLE:
            return f"{to_print} : The value is a single value."
        if self is ObjScnAttrValues.SCN_ATTR_VALUE_LIST_CONCATENATED:
            return f"{to_print} : The value is a list. This list of values are "\
                   + "expected to be concatenated. All values will be concatenated " \
                   + "and treated as an unique value."
        if self is ObjScnAttrValues.SCN_ATTR_VALUE_LIST_DISTRIBUTED:
            return f"{to_print} : The value is a list. This list of values are "\
                   + "expected to be distributed. Each value will be managed "\
                   + "separately."


class ObjScnCmd(Enum):
    SCN_CMD_OUTPUT_FILE = "output.log"
    SCN_CMD_CONFIG_FILE = "files/alternative.cgpr"

    def doc(self):
        to_print = f"   - {self.name}"
        if self is ObjScnCmd.SCN_CMD_OUTPUT_FILE:
            return f"{to_print} : "
        if self is ObjScnCmd.SCN_CMD_CONFIG_FILE:
            return f"{to_print} : "


class ObjScnAttrSubstPattern(Enum):
    SCN_ATTR_SUBSTITUTE_PATTERN = "<attr_val>"
    SCN_ATTR_ALT_SUBST_PATTERN_ALL = "<attr_all>"
    SCN_ATTR_ALT_SUBST_PATTERN_PREFIX = "<attr_prefix>"
    SCN_ATTR_ALT_SUBST_PATTERN_NAME = "<attr_name>"
    SCN_ATTR_ALT_SUBST_PATTERN_SUFFIX = "<attr_suffix>"
    SCN_PRJ_SUBSTITUTE_PATTERN = "<prj_val>"

    def doc(self):
        to_print = f"- {self.name}"
        if self is ObjScnAttrSubstPattern.SCN_ATTR_SUBSTITUTE_PATTERN:
            return f"{to_print} : This pattern will be replaced by the attribute "\
                   + "value found in the linked project file."
        if self is ObjScnAttrSubstPattern.SCN_ATTR_ALT_SUBST_PATTERN_ALL:
            return f"{to_print} : This pattern will be replaced by the attribute "\
                   + "alternative value defined for the testcase. The tuple will be "\
                   + "concatenated. Setting Opt.SCN_OPTION_USE_ALT_ATTR_VALUE is "\
                   + "mandatory."
        if self is ObjScnAttrSubstPattern.SCN_ATTR_ALT_SUBST_PATTERN_PREFIX:
            return f"{to_print} : This pattern will be replaced by the attribute "\
                   + "alternative value defined for the testcase. Only the prefix "\
                   + "and name of the tuple will be replaced. "\
                   + "Setting Opt.SCN_OPTION_USE_ALT_ATTR_VALUE is mandatory."
        if self is ObjScnAttrSubstPattern.SCN_ATTR_ALT_SUBST_PATTERN_NAME:
            return f"{to_print} : This pattern will be replaced by the attribute "\
                   + "alternative value defined for the testcase. Only the name "\
                   + "of the tuple will be replaced. "\
                   + "Setting Opt.SCN_OPTION_USE_ALT_ATTR_VALUE is mandatory."
        if self is ObjScnAttrSubstPattern.SCN_ATTR_ALT_SUBST_PATTERN_SUFFIX:
            return f"{to_print} : This pattern will be replaced by the attribute "\
                   + "alternative value defined for the testcase. Only the name "\
                   + "and prefix of the tuple will be replaced. "\
                   + "Setting Opt.SCN_OPTION_USE_ALT_ATTR_VALUE is mandatory."
        if self is ObjScnAttrSubstPattern.SCN_PRJ_SUBSTITUTE_PATTERN:
            return f"{to_print} : This pattern will be replaced by the testcase "\
                   + "project file. This pattern is used for launching test commands "\
                   + "found in driver_constants.SCN_TOOLS_CMD."


class ObjOptions(IntFlag):
    SCN_OPTION_USE_CGPR = 1
    SCN_OPTION_USE_ALT_ATTR_VALUE = 2
    SCN_OPTION_CONVERT_FILE_TO_DIRNAME = 4
    SCN_OPTION_FULL_STRING_COMPARISION = 8
    SCN_OPTION_RES_AS_A_FULL_STRING = 16
    SCN_OPTION_HIDE_PATH_TO_FILE = 32
    SCN_OPTION_HIDE_EXPECTED_RES = 64

    def doc(self):
        to_print = f"- {self.name}"
        if self is ObjOptions.SCN_OPTION_USE_CGPR:
            return f"{to_print} : Defines to the driver that the test command must "\
                   + "use a config project file."
        if self is ObjOptions.SCN_OPTION_USE_ALT_ATTR_VALUE:
            return f"{to_print} : Defines to the driver that an alternative value for "\
                   + "the attribute is defined by the user. This value will be used "\
                   + "instead of the one read from the testcase project file."
        if self is ObjOptions.SCN_OPTION_CONVERT_FILE_TO_DIRNAME:
            return f"{to_print} : Defines to the driver that when the output of a "\
                   + "command is set to SCN_OUTPUT_FILE_LIST it must only use the "\
                   + "dirname of those files."
        if self is ObjOptions.SCN_OPTION_FULL_STRING_COMPARISION:
            return f"{to_print} : Defines to the driver that when checking values in "\
                   + "the output it is making a full string match and not a partial "\
                   + "match. (E.g. : Checking a relative filename in a list of "\
                   + "absolute paths)."
        if self is ObjOptions.SCN_OPTION_RES_AS_A_FULL_STRING:
            return f"{to_print} : Defines to the driver that it should not parse the "\
                   + "output but consider it a a full string."
        if self is ObjOptions.SCN_OPTION_HIDE_PATH_TO_FILE:
            return f"{to_print} : Defines to the driver that it should not output "\
                   + "full paths in the log. (E.g. : The test is about source files, "\
                   + "and logging such path would fail the CI and the output would "\
                   + "be different locally and on the CI host."
        if self is ObjOptions.SCN_OPTION_HIDE_EXPECTED_RES:
            return f"{to_print} : Defines to the driver that it should not output "\
                   + "certain things in the log due to variability issues between "\
                   + "different hosts."


phase = ObjScnPhase
tool = ObjScnTool
output = ObjScnOutput
cmd = ObjScnCmd
pattern = ObjScnAttrSubstPattern

""" This structure describes every command line that can be launched
    during a testcase.
"""
SCN_TOOLS_CMD = {
    f"{tool.SCN_TOOL_CUSTOM}.{phase.SCN_PHASE_CUSTOM_SLOT_1}":
        {"cmd": None, "output": None},
    f"{tool.SCN_TOOL_CUSTOM}.{phase.SCN_PHASE_CUSTOM_SLOT_2}":
        {"cmd": None, "output": None},
    f"{tool.SCN_TOOL_CUSTOM}.{phase.SCN_PHASE_CUSTOM_SLOT_3}":
        {"cmd": None, "output": None},
    f"{tool.SCN_TOOL_CUSTOM}.{phase.SCN_PHASE_CUSTOM_SLOT_4}":
        {"cmd": None, "output": None},
    f"{tool.SCN_TOOL_CUSTOM}.{phase.SCN_PHASE_CUSTOM_SLOT_5}":
        {"cmd": None, "output": None},
    f"{tool.SCN_TOOL_CUSTOM}.{phase.SCN_PHASE_CUSTOM_SLOT_6}":
        {"cmd": None, "output": None},
    f"{tool.SCN_TOOL_CUSTOM}.{phase.SCN_PHASE_CUSTOM_SLOT_7}":
        {"cmd": None, "output": None},
    f"{tool.SCN_TOOL_CUSTOM}.{phase.SCN_PHASE_CUSTOM_SLOT_8}":
        {"cmd": None, "output": None},
    f"{tool.SCN_TOOL_GPRBUILD}.{phase.SCN_PHASE_GPRBUILD_COMPILATION}":
        {"cmd": ['gprbuild', '-c', f'--build-script={cmd.SCN_CMD_OUTPUT_FILE.value}',
                 f'{pattern.SCN_PRJ_SUBSTITUTE_PATTERN.value}'],
         "output": output.SCN_OUTPUT_CMD_LINE},
    f"{tool.SCN_TOOL_GPRBUILD}.{phase.SCN_PHASE_GPRBUILD_BIND}":
        {"cmd": ['gprbuild', '-b', f'--build-script={cmd.SCN_CMD_OUTPUT_FILE.value}',
                 f'{pattern.SCN_PRJ_SUBSTITUTE_PATTERN.value}'],
         "output": output.SCN_OUTPUT_CMD_LINE},
    f"{tool.SCN_TOOL_GPRBUILD}.{phase.SCN_PHASE_GPRBUILD_LINK}":
        {"cmd": ['gprbuild', '-l', f'--build-script={cmd.SCN_CMD_OUTPUT_FILE.value}',
                 f'{pattern.SCN_PRJ_SUBSTITUTE_PATTERN.value}'],
         "output": output.SCN_OUTPUT_CMD_LINE},
    f"{tool.SCN_TOOL_GPRBUILD}.{phase.SCN_PHASE_GPRBUILD_ALL}":
        {"cmd": ['gprbuild', f'--build-script={cmd.SCN_CMD_OUTPUT_FILE.value}',
                 f'{pattern.SCN_PRJ_SUBSTITUTE_PATTERN.value}'],
         "output": output.SCN_OUTPUT_CMD_LINE},
    f"{tool.SCN_TOOL_GPRCONFIG}.{phase.SCN_PHASE_NONE}":
        {"cmd": ['gprconfig', '--batch', '--config=Ada', '-o',
                 f'{cmd.SCN_CMD_CONFIG_FILE.value}'],
         "output": None},
    f"{tool.SCN_TOOL_GPRCLEAN}.{phase.SCN_PHASE_NONE}":
        {"cmd": ['gprclean', '-p', '-r', f'{pattern.SCN_PRJ_SUBSTITUTE_PATTERN.value}'],
         "output": None},
    f"{tool.SCN_TOOL_GPRCLEAN}.{phase.SCN_PHASE_GPRCLEAN_ALL}":
        {"cmd": ['gprclean', '-p', '-r', f'{pattern.SCN_PRJ_SUBSTITUTE_PATTERN.value}',
                 '-n'],
         "output": output.SCN_OUTPUT_CMD_LINE},
    f"{tool.SCN_TOOL_GPRLS}.{phase.SCN_PHASE_GPRLS_SOURCE_PARSING}":
        {"cmd": ['gprls', '--source-parser', '-s',
                 f'{pattern.SCN_PRJ_SUBSTITUTE_PATTERN.value}'],
         "output": output.SCN_OUTPUT_FILE_LIST}
}
