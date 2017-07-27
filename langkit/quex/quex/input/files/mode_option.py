import quex.input.files.counter             as     counter
import quex.input.regular_expression.core   as     regular_expression
from   quex.input.files.parser_data.counter import CounterSetupLineColumn_Default
from   quex.input.code.base                 import SourceRef
from   quex.engine.misc.tools               import all_isinstance
from   quex.engine.misc.tools               import typed, \
                                                   flatten_list_of_lists
import quex.engine.misc.error               as     error
from   quex.engine.misc.file_in             import skip_whitespace, \
                                                   read_identifier, \
                                                   EndOfStreamException

from   quex.blackboard import mode_description_db
import quex.blackboard as     blackboard

from   collections import namedtuple
import types

def __get_mode_name_list():
    return mode_description_db.keys()

class SkipRangeData(dict): 
    def __init__(self, OpenerPattern, OpenerSequence,
                 CloserPattern, CloserSequence):
        self["opener_pattern"]  = OpenerPattern
        self["opener_sequence"] = OpenerSequence
        self["closer_pattern"]  = CloserPattern
        self["closer_sequence"] = CloserSequence

#-----------------------------------------------------------------------------------------
# mode_option_info_db: Information about properties of mode options.
#-----------------------------------------------------------------------------------------
class ModeOptionInfo:
    """A ModeOptionInfo is an element of mode_option_info_db."""
    def __init__(self, MultiDefinitionF, OverwriteF, Domain=None, Default=None, ListF=False):
        assert type(MultiDefinitionF) == bool
        assert type(OverwriteF) == bool
        assert type(ListF) == bool
        # self.name = Option see comment above
        self.multiple_definition_f = MultiDefinitionF # Can be defined multiple times?
        self.derived_overwrite_f   = OverwriteF       # Does derived definition overwrite base mode's definition?
        self.domain                = Domain
        self.__content_list_f      = ListF
        self.__default_value       = Default

    def single_setting_f(self):
        """If the option can be defined multiple times and is not overwritten, 
        then there is only one setting for a given name. Otherwise not."""
        return (not self.multiple_definition_f) or self.derived_overwrite_f

    def content_is_list(self):
        return self.__content_list_f

    def default_setting(self, ModeName):
        if self.__default_value is None:
            return None

        if isinstance(self.__default_value, types.FunctionType):
            content = self.__default_value()
        else:
            content = self.__default_value

        return OptionSetting(content, SourceRef(), ModeName)

OptionSetting = namedtuple("OptionSetting", ("value", "sr", "mode_name"))

mode_option_info_db = {
   # -- a mode can be inheritable or not or only inheritable. if a mode
   #    is only inheritable it is not printed on its on, only as a base
   #    mode for another mode. default is 'yes'
   "inheritable":       ModeOptionInfo(False, True, ["no", "yes", "only"], Default="yes"),
   # -- a mode can restrict the possible modes to exit to. this for the
   #    sake of clarity. if no exit is explicitly mentioned all modes are
   #    possible. if it is tried to transit to a mode which is not in
   #    the list of explicitly stated exits, an error occurs.
   #    entrys work respectively.
   "exit":              ModeOptionInfo(True, False, Default=__get_mode_name_list, ListF=True),
   "entry":             ModeOptionInfo(True, False, Default=__get_mode_name_list, ListF=True),
   # -- a mode can restrict the exits and entrys explicitly mentioned then, a
   #    derived mode cannot add new exits or entrys.
   "restrict":          ModeOptionInfo(True, False, ["exit", "entry"], Default=[], ListF=True),
   # -- a mode can have 'skippers' that effectively skip ranges that are out 
   #    of interest.
   "skip":              ModeOptionInfo(True, False), # "multiple: RE-character-set
   "skip_range":        ModeOptionInfo(True, False), # "multiple: RE-character-string RE-character-string
   "skip_nested_range": ModeOptionInfo(True, False), # "multiple: RE-character-string RE-character-string
   # -- indentation setup information
   "indentation":       ModeOptionInfo(False, False),
   # --line/column counter information
   "counter":           ModeOptionInfo(False, False, Default=CounterSetupLineColumn_Default),
}

class OptionDB(dict):
    """Database of Mode Options
    ---------------------------------------------------------------------------

                      option name --> [ OptionSetting ]

    If the 'mode_option_info_db[option_name]' mentions that there can be 
    no multiple definitions or if the options can be overwritten than the 
    list of OptionSetting-s must be of length '1' or the list does not exist.

    ---------------------------------------------------------------------------
    """
    def get(self, Key):         
        assert False, "Not to be used, but result would be '%s'" % dict.get(self, Key) 
    def __getitem__(self, Key): 
        assert False, "Not to be used, but result would be '%s'" % dict.__getitem__(self, Key)
    def __setitem__(self, Key): assert False # Not to be used

    @classmethod
    def from_BaseModeSequence(cls, BaseModeSequence):
        # BaseModeSequence[-1] = mode itself
        mode_name = BaseModeSequence[-1].name

        def setting_list_iterable(BaseModeSequence):
            for mode_descr in BaseModeSequence:
                option_db = mode_descr.option_db
                for name, info in mode_option_info_db.iteritems():
                    setting_list = option_db.__get_setting_list(name)
                    if setting_list is None: continue
                    assert    (not mode_option_info_db[name].single_setting_f()) \
                           or (len(setting_list) == 1)  
                    yield name, setting_list

        result = cls()
        for name, setting_list in setting_list_iterable(BaseModeSequence):
            result.__enter_setting_list(name, setting_list) 

        # Options which have not been set (or inherited) are set to the default value.
        for name, info in mode_option_info_db.iteritems():
            if name in result: continue
            if info.default_setting(mode_name) is None: continue
            result.__enter_setting(name, info.default_setting(mode_name))

        return result

    def enter(self, Name, Value, SourceReference, ModeName):
        """Enters a new definition of a mode option as it comes from the parser.
        At this point, it is assumed that the OptionDB belongs to one single
        ModeDescription and not to a base-mode accumulated Mode. Thus, one
        option can only be set once, otherwise an error is notified.
        """
        global mode_option_info_db
        # The 'verify_word_in_list()' call must have ensured that the following holds
        assert Name in mode_option_info_db

        # Is the option of the appropriate value?
        info = mode_option_info_db[Name]
        if info.domain is not None and Value not in info.domain:
            error.log("Tried to set value '%s' for option '%s'. " % (Value, Name) + \
                      "Though, possible for this option are only: %s." % repr(info.domain)[1:-1], 
                      SourceReference)

        setting = OptionSetting(Value, SourceReference, ModeName)
        self.__enter_setting(Name, setting)

    @typed(Setting=(None, OptionSetting))
    def __enter_setting(self, Name, Setting):
        """During building of a ModeDescription. Enters a OptionSetting safely into the 
        OptionDB. It checks whether it is already present.
        """
        info = mode_option_info_db[Name]
        assert (not info.content_is_list()) or     isinstance(Setting.value, list)
        assert (    info.content_is_list()) or not isinstance(Setting.value, list)

        if Name not in self:             dict.__setitem__(self, Name, [ Setting ])
        elif info.multiple_definition_f: dict.__getitem__(self, Name).append(Setting)
        else:                            self.__error_double_definition(Name, Setting)

    @typed(SettingList=[OptionSetting])
    def __enter_setting_list(self, Name, SettingList):
        """During construction of a 'real' Mode object from including consideration
        of base modes. Enter the given setting list, or extend.
        """
        info = mode_option_info_db[Name]
        if   Name not in self:           dict.__setitem__(self, Name, SettingList)
        elif info.derived_overwrite_f:   dict.__setitem__(self, Name, SettingList)
        elif info.multiple_definition_f: dict.__getitem__(self, Name).extend(SettingList)
        else:                            self.__error_double_definition(Name, SettingList[0])

    def __get_setting_list(self, Name):
        """RETURNS: [ OptionSetting ] for a given option's Name.

        This function does additional checks for consistency.
        """
        setting_list = dict.get(self, Name)
        if setting_list is None: return None

        assert isinstance(setting_list, list) 
        assert all_isinstance(setting_list, OptionSetting)
        assert (not mode_option_info_db[Name].single_setting_f()) or len(setting_list) == 1
        return setting_list

    def value(self, Name):
        """Only scalar options can be asked about their 'value'."""
        setting_list = self.__get_setting_list(Name)
        if setting_list is None: return None
        assert mode_option_info_db[Name].single_setting_f()

        scalar_value = setting_list[0].value
        assert not isinstance(scalar_value, list)
        return scalar_value

    def value_sequence(self, Name):
        setting_list = self.__get_setting_list(Name)
        if setting_list is None: return None

        assert not mode_option_info_db[Name].single_setting_f(), \
               "'%s' is not supposed to be a value sequence." % Name
        return [ x.value for x in setting_list ]

    def value_list(self, Name):
        """The content of a value is a sequence, and the return value of this
        function is a concantinated list of all listed option setting values.
        """
        setting_list = self.__get_setting_list(Name)
        if setting_list is None: return None

        info = mode_option_info_db[Name]
        if info.content_is_list():
            result = flatten_list_of_lists(
                x.value for x in setting_list
            )
        else:
            result = [ x.value for x in setting_list ]

        return result

    def __error_double_definition(self, Name, OptionNow):
        assert isinstance(OptionNow, OptionSetting)
        OptionBefore = dict.__getitem__(self, Name)[0]
        assert isinstance(OptionBefore, OptionSetting)

        txt = "Option '%s' defined twice in " % Name
        if OptionBefore.mode_name == OptionNow.mode_name:
            txt += "mode '%s'." % OptionNow.mode_name
        else:
            txt += "inheritance tree of mode '%s'." % OptionNow.mode_name
        error.log(txt, OptionNow.sr, DontExitF=True) 

        txt = "Previous definition was here"
        if OptionBefore.mode_name == OptionNow.mode_name:
            txt += " in mode '%s'." % OptionBefore.mode_name
        else:
            txt += "."
        error.log(txt, OptionBefore.sr.file_name, OptionBefore.sr.line_n)

def parse(fh, new_mode):
    source_reference = SourceRef.from_FileHandle(fh)

    identifier = read_option_start(fh)
    if identifier is None: return False

    error.verify_word_in_list(identifier, mode_option_info_db.keys(),
                              "mode option", fh)

    if   identifier == "skip":
        value = __parse_skip_option(fh, new_mode, identifier)

    elif identifier in ["skip_range", "skip_nested_range"]:
        value = __parse_range_skipper_option(fh, identifier, new_mode)
        
    elif identifier == "indentation":
        value = counter.parse_indentation(fh)
        value.set_containing_mode_name(new_mode.name)
        blackboard.required_support_indentation_count_set()

    elif identifier == "counter":
        value = counter.parse_line_column_counter(fh)

    elif identifier in ("entry", "exit", "restrict"):
        value = read_option_value(fh, ListF=True) # A 'list' of strings
    else:
        value = read_option_value(fh)             # A single string

    # Finally, set the option
    new_mode.option_db.enter(identifier, value, source_reference, new_mode.name)
    return True

def __parse_skip_option(fh, new_mode, identifier):
    """A skipper 'eats' characters at the beginning of a pattern that belong to
    a specified set of characters. A useful application is most probably the
    whitespace skipper '[ \t\n]'. The skipper definition allows quex to
    implement a very effective way to skip these regions."""

    pattern, trigger_set = regular_expression.parse_character_set(fh, ">")

    skip_whitespace(fh)

    if fh.read(1) != ">":
        error.log("missing closing '>' for mode option '%s'." % identifier, fh)
    elif trigger_set.is_empty():
        error.log("Empty trigger set for skipper." % identifier, fh)

    return pattern, trigger_set

def __parse_range_skipper_option(fh, identifier, new_mode):
    """A non-nesting skipper can contain a full fledged regular expression as opener,
    since it only effects the trigger. Not so the nested range skipper-see below.
    """

    # Range state machines only accept 'strings' not state machines
    skip_whitespace(fh)
    opener_pattern, opener_sequence = regular_expression.parse_character_string(fh, ">")
    skip_whitespace(fh)
    closer_pattern, closer_sequence = regular_expression.parse_character_string(fh, ">")

    # -- closer
    skip_whitespace(fh)
    if fh.read(1) != ">":
        error.log("missing closing '>' for mode option '%s'" % identifier, fh)
    elif len(opener_sequence) == 0:
        error.log("Empty sequence for opening delimiter.", fh)
    elif len(closer_sequence) == 0:
        error.log("Empty sequence for closing delimiter.", fh)

    return SkipRangeData(opener_pattern, opener_sequence, \
                         closer_pattern, closer_sequence)

def read_option_start(fh):
    skip_whitespace(fh)

    # (*) base modes 
    if fh.read(1) != "<": 
        ##fh.seek(-1, 1) 
        return None

    skip_whitespace(fh)
    identifier = read_identifier(fh, OnMissingStr="Missing identifer after start of mode option '<'").strip()

    skip_whitespace(fh)
    if fh.read(1) != ":": error.log("missing ':' after option name '%s'" % identifier, fh)
    skip_whitespace(fh)

    return identifier

def read_option_value(fh, ListF=False):

    position = fh.tell()

    value = ""
    depth = 1
    while 1 + 1 == 2:
        try: 
            letter = fh.read(1)
        except EndOfStreamException:
            fh.seek(position)
            error.log("missing closing '>' of mode option.", fh)

        if letter == "<": 
            depth += 1
        if letter == ">": 
            depth -= 1
            if depth == 0: break
        value += letter

    if not ListF: return value.strip()
    else:         return [ x.strip() for x in value.split() ]


