from enum import IntFlag, Enum


class ObjScnTool(Enum):
    SCN_TOOL_CUSTOM = 0
    SCN_TOOL_GPRBUILD = 1
    SCN_TOOL_GPRCLEAN = 2
    SCN_TOOL_GPRDOC = 3
    SCN_TOOL_GPRCONFIG = 4

    def tool_name(self):
        return self.name.lower().split("_")[-1]


class ObjScnPhase(Enum):
    SCN_PHASE_NONE = 0
    SCN_PHASE_GPRBUILD_COMPILATION = 1
    SCN_PHASE_GPRBUILD_BIND = 2
    SCN_PHASE_GPRBUILD_LINK = 3
    SCN_PHASE_CUSTOM_SLOT_1 = 101
    SCN_PHASE_CUSTOM_SLOT_2 = 102
    SCN_PHASE_CUSTOM_SLOT_3 = 103
    SCN_PHASE_CUSTOM_SLOT_4 = 104
    SCN_PHASE_CUSTOM_SLOT_5 = 105
    SCN_PHASE_CUSTOM_SLOT_6 = 106
    SCN_PHASE_CUSTOM_SLOT_7 = 107
    SCN_PHASE_CUSTOM_SLOT_8 = 108


class ObjScnCaseValue(Enum):
    SCN_CASE_VALUE_DEFAULT = 0
    SCN_CASE_VALUE_DEF = 1
    SCN_CASE_VALUE_UNDEF = 2
    SCN_CASE_VALUE_INVALID = 4


class ObjScnRes(Enum):
    SCN_RES_FOR_NONE = 0
    SCN_RES_FOR_ANY = 1
    SCN_RES_FOR_ALL = 2


class ObjScnAttrValues(Enum):
    SCN_ATTR_VALUES_UNIQUE = 0
    SCN_ATTR_VALUES_CONCATENATED = 1
    SCN_ATTR_VALUES_DISTRIBUTED = 2


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


osp = ObjScnPhase
ost = ObjScnTool
osc = ObjScnCmd
osa = ObjScnAttrSubstPattern

""" This structure describes every command line that can be launched
    during a testcase.
"""
SCN_TOOLS_CMD = {
    f"{ost.SCN_TOOL_CUSTOM}.{osp.SCN_PHASE_CUSTOM_SLOT_1}": None,
    f"{ost.SCN_TOOL_CUSTOM}.{osp.SCN_PHASE_CUSTOM_SLOT_2}": None,
    f"{ost.SCN_TOOL_CUSTOM}.{osp.SCN_PHASE_CUSTOM_SLOT_3}": None,
    f"{ost.SCN_TOOL_CUSTOM}.{osp.SCN_PHASE_CUSTOM_SLOT_4}": None,
    f"{ost.SCN_TOOL_CUSTOM}.{osp.SCN_PHASE_CUSTOM_SLOT_5}": None,
    f"{ost.SCN_TOOL_CUSTOM}.{osp.SCN_PHASE_CUSTOM_SLOT_6}": None,
    f"{ost.SCN_TOOL_CUSTOM}.{osp.SCN_PHASE_CUSTOM_SLOT_7}": None,
    f"{ost.SCN_TOOL_CUSTOM}.{osp.SCN_PHASE_CUSTOM_SLOT_8}": None,
    f"{ost.SCN_TOOL_GPRBUILD}.{osp.SCN_PHASE_GPRBUILD_COMPILATION}":
        ['gprbuild', '-c', f'--build-script={osc.SCN_CMD_OUTPUT_FILE.value}',
         f'{osa.SCN_PRJ_SUBSTITUTE_PATTERN.value}'],
    f"{ost.SCN_TOOL_GPRBUILD}.{osp.SCN_PHASE_GPRBUILD_BIND}":
        ['gprbuild', '-b', f'--build-script={osc.SCN_CMD_OUTPUT_FILE.value}',
         f'{osa.SCN_PRJ_SUBSTITUTE_PATTERN.value}'],
    f"{ost.SCN_TOOL_GPRBUILD}.{osp.SCN_PHASE_GPRBUILD_LINK}":
        ['gprbuild', '-l', f'--build-script={osc.SCN_CMD_OUTPUT_FILE.value}',
         f'{osa.SCN_PRJ_SUBSTITUTE_PATTERN.value}'],
    f"{ost.SCN_TOOL_GPRCONFIG}.{osp.SCN_PHASE_NONE}":
        ['gprconfig', '--batch', '--config=Ada', '-o',
         f'{osc.SCN_CMD_CONFIG_FILE.value}'],
    f"{ost.SCN_TOOL_GPRCLEAN}.{osp.SCN_PHASE_NONE}":
        ['gprclean', '-p', '-r', f'{osa.SCN_PRJ_SUBSTITUTE_PATTERN.value}']
}
