from enum import IntFlag, Enum
from e3.env import Env


class ObjScnTool(Enum):
    SCN_TOOL_CUSTOM = 0
    SCN_TOOL_GPRBUILD = 1
    SCN_TOOL_GPRCLEAN = 2
    SCN_TOOL_GPRDOC = 3
    SCN_TOOL_GPRCONFIG = 4
    SCN_TOOL_GPRLS = 5

    def tool_name(self):
        return self.name.lower().split("_")[-1]


class ObjScnPhase(Enum):
    SCN_PHASE_NONE = 0
    SCN_PHASE_GPRBUILD_COMPILATION = 1
    SCN_PHASE_GPRBUILD_BIND = 2
    SCN_PHASE_GPRBUILD_LINK = 3
    SCN_PHASE_GPRLS_SOURCE_PARSING = 4
    SCN_PHASE_CUSTOM_SLOT_1 = 101
    SCN_PHASE_CUSTOM_SLOT_2 = 102
    SCN_PHASE_CUSTOM_SLOT_3 = 103
    SCN_PHASE_CUSTOM_SLOT_4 = 104
    SCN_PHASE_CUSTOM_SLOT_5 = 105
    SCN_PHASE_CUSTOM_SLOT_6 = 106
    SCN_PHASE_CUSTOM_SLOT_7 = 107
    SCN_PHASE_CUSTOM_SLOT_8 = 108


class ObjScnExternalTool(Enum):
    if Env().host.platform.endswith('windows'):
        SCN_CMD_GCC = "gcc.exe'"
    else:
        SCN_CMD_GCC = "gcc"


class ObjScnOutput(Enum):
    SCN_OUTPUT_FILE_LIST = 0
    SCN_OUTPUT_CMD_LINE = 1


class ObjScnCaseValue(Enum):
    SCN_CASE_VALUE_DEFAULT = 0
    SCN_CASE_VALUE_DEF = 1
    SCN_CASE_VALUE_UNDEF = 2
    SCN_CASE_VALUE_INVALID = 4


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


class ObjScnAttrValues(Enum):
    SCN_ATTR_VALUES_UNIQUE = 0
    SCN_ATTR_VALUES_CONCATENATED = 1
    SCN_ATTR_VALUES_DISTRIBUTED = 2

    def unique(self):
        if self is ObjScnAttrValues.SCN_ATTR_VALUES_UNIQUE:
            return True
        return False

    def concatenated(self):
        if self is ObjScnAttrValues.SCN_ATTR_VALUES_CONCATENATED:
            return True
        return False

    def distributed(self):
        if self is ObjScnAttrValues.SCN_ATTR_VALUES_DISTRIBUTED:
            return True
        return False


class ObjScnCmd(Enum):
    SCN_CMD_OUTPUT_FILE = "output.log"
    SCN_CMD_CONFIG_FILE = "files/alternative.cgpr"


class ObjScnAttrSubstPattern(Enum):
    SCN_ATTR_SUBSTITUTE_PATTERN = "<attr_val>"
    SCN_ATTR_ALT_SUBST_PATTERN_ALL = "<attr_all>"
    SCN_ATTR_ALT_SUBST_PATTERN_PREFIX = "<attr_prefix>"
    SCN_ATTR_ALT_SUBST_PATTERN_NAME = "<attr_name>"
    SCN_ATTR_ALT_SUBST_PATTERN_SUFFIX = "<attr_suffix>"
    SCN_PRJ_SUBSTITUTE_PATTERN = "<prj_val>"


class ObjOptions(IntFlag):
    """Allows the addition of a config file to GPRtools command."""
    SCN_OPTION_USE_CGPR = 1
    """Allows to manually give the expected attribute value to the driver."""
    SCN_OPTION_USE_ALT_ATTR_VALUE = 2
    """Allows to convert filename into dirname. Only taken into account if
       the output kind is defined as SCN_OUTPUT_FILE_LIST.
    """
    SCN_OPTION_CONVERT_FILE_TO_DIRNAME = 4
    """Allows to compare equality between the actual and expected outputs instead of
       looking for expected substring presence into the actual output.
    """
    SCN_OPTION_FULL_STRING_COMPARISION = 8
    """Allows to consider expected behavior to be treated as a string in order to
       escape spaces.
    """
    SCN_OPTION_RES_AS_A_FULL_STRING = 16
    """Allows to hide file absolute path in the expected behavior.
    """
    SCN_OPTION_HIDE_PATH_TO_FILE = 32


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
    f"{tool.SCN_TOOL_GPRCONFIG}.{phase.SCN_PHASE_NONE}":
        {"cmd": ['gprconfig', '--batch', '--config=Ada', '-o',
                 f'{cmd.SCN_CMD_CONFIG_FILE.value}'],
         "output": None},
    f"{tool.SCN_TOOL_GPRCLEAN}.{phase.SCN_PHASE_NONE}":
        {"cmd": ['gprclean', '-p', '-r', f'{pattern.SCN_PRJ_SUBSTITUTE_PATTERN.value}'],
         "output": None},
    f"{tool.SCN_TOOL_GPRLS}.{phase.SCN_PHASE_GPRLS_SOURCE_PARSING}":
        {"cmd": ['gprls', '--source-parser', '-s',
                 f'{pattern.SCN_PRJ_SUBSTITUTE_PATTERN.value}'],
         "output": output.SCN_OUTPUT_FILE_LIST}
}
