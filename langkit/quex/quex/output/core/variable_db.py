from copy import copy

class Variable:
    def __init__(self, Name, Type, ElementN, InitialValue, Condition=None, ConditionNegatedF=False, PriorityF=False):
        """ElementN is None --> scalar value
                    is int  --> array of dimension ElementN
        """
        assert type(ElementN) in [int, long] or None == ElementN, \
               "ElementN is of type '%s'" % repr(type(ElementN))

        self.name                = Name
        self.variable_type       = Type
        self.initial_value       = InitialValue
        self.element_n           = ElementN
        self.condition           = Condition
        self.condition_negated_f = ConditionNegatedF
        # Some variables need to be defined before others --> Set some to 'prior'
        self.priority_f          = PriorityF


candidate_db = {
# Name                             Type(0),                         InitialValue(2),               PriorityF(3)
"input":                          ["QUEX_TYPE_CHARACTER",           "(QUEX_TYPE_CHARACTER)(0x00)", False],
"target_state_index":             ["QUEX_TYPE_GOTO_LABEL",          "QUEX_GOTO_LABEL_VOID",        False],
"target_state_else_index":        ["QUEX_TYPE_GOTO_LABEL",          "QUEX_GOTO_LABEL_VOID",        False],
"last_acceptance":                ["QUEX_TYPE_ACCEPTANCE_ID",       None,                          False],
"PositionRegisterN":              ["const size_t",                  None,                          False],
"position":                       ["QUEX_TYPE_CHARACTER_POSITION",  None,                          False],
"pre_context_%i_fulfilled_f":     ["int",                           "0",                           False], 
"counter":                        ["size_t",                        "0",                           False],
"end_of_core_pattern_position":   ["QUEX_TYPE_CHARACTER_POSITION",  "((QUEX_TYPE_CHARACTER*)0x0)", False],
#                                 
# (*) Path Compression
"path_iterator":                  ["const QUEX_TYPE_CHARACTER*",       "((const QUEX_TYPE_CHARACTER*)0x0)", False],
"path_end_state":                 ["QUEX_TYPE_GOTO_LABEL",             "QUEX_GOTO_LABEL_VOID",  False], 
"path_walker_%i_path_%i":         ["const QUEX_TYPE_CHARACTER* const", None,                                False],
"path_walker_%i_path_base":       ["const QUEX_TYPE_CHARACTER",        None,                                True],
"path_walker_%i_path_%i_states":  ["const QUEX_TYPE_GOTO_LABEL* const",None,                                False],
"path_walker_%i_state_base":      ["const QUEX_TYPE_GOTO_LABEL",       None,                                True],
"path_walker_%i_reference":       ["const QUEX_TYPE_CHARACTER* const", None,                                False],
#
# (*) Template Compression
"state_key":                                    ["ptrdiff_t",                     "(ptrdiff_t)0",           False],
"template_%i_target_%i":                        ["const QUEX_TYPE_GOTO_LABEL",    None,                     False],
"template_%i_map_state_key_to_recursive_entry": ["const QUEX_TYPE_GOTO_LABEL",    None,                     False],
#
# (*) Skipper etc.
"reference_p":                    ["QUEX_TYPE_CHARACTER_POSITION", "(QUEX_TYPE_CHARACTER_POSITION)0x0", False],
"character_begin_p":              ["QUEX_TYPE_CHARACTER_POSITION", "(QUEX_TYPE_CHARACTER_POSITION)0x0", False],
"text_end":                       ["QUEX_TYPE_CHARACTER*",         "(QUEX_TYPE_CHARACTER*)0x0",         False],
#     Character Set Skipper:
"Skipper%i":                      ["const QUEX_TYPE_CHARACTER",    None,                                False],
"Skipper%iL":                     ["const size_t",                 None,                                False],
#     Range Skipper (from string to string)
"Skipper%i_Opener":               ["const QUEX_TYPE_CHARACTER",    None,                                True],
"Skipper%i_OpenerEnd":            ["const QUEX_TYPE_CHARACTER*",   None,                                False],
"Skipper%i_Opener_it":            ["const QUEX_TYPE_CHARACTER*",   None,                                False],
"Skipper%i_Closer":               ["const QUEX_TYPE_CHARACTER",    None,                                True],
"Skipper%i_CloserEnd":            ["const QUEX_TYPE_CHARACTER*",   None,                                False],
"Skipper%i_Closer_it":            ["const QUEX_TYPE_CHARACTER*",   None,                                False],
}

def enter(local_variable_db, Name, InitialValue=None, ElementN=None, Condition=None, ConditionNegatedF=False, Index=None):
    x = candidate_db[Name]

    if Index is not None: Name = Name % Index

    Type      = x[0]
    PriorityF = x[2]
    if InitialValue is None: InitialValue = x[1]

    local_variable_db[Name] = Variable(Name, Type, ElementN, InitialValue, Condition, ConditionNegatedF, PriorityF)

class VariableDB:

    def __init__(self, InitialDB=None):
        self.__db = {}
        self.init(InitialDB)

    def init(self, InitialDB=None):
        if InitialDB is None: self.__db.clear()
        else:                 self.__db = copy(InitialDB)

    def update(self, Other):
        self.__db.update(Other)

    def get(self):
        return self.__db

    def has_key(self, Key):
        return self.__db.has_key(Key)

    def __enter(self, Name, Type, ElementN, InitialValues, Condition, ConditionNegatedF, PriorityF):
        # (*) Determine unique key for Name, Condition, and ConditionNegatedF.
        if Condition is None: 
            key = Name
        else:
            key = "%s/%s" % (Condition, Name)
            if ConditionNegatedF: key = "!" + key

        # (*) Enter the variable into the database
        self.__db[key] = Variable(Name, Type, ElementN, InitialValues, Condition, ConditionNegatedF, PriorityF)

    def __condition(self, Condition_ComputedGoto):
        if Condition_ComputedGoto is None:
            return None, None
        else:
            assert type(Condition_ComputedGoto) == bool
            return "QUEX_OPTION_COMPUTED_GOTOS", not Condition_ComputedGoto

    def require(self, Name, Initial=None, Index=None, Condition_ComputedGoto=None, Type=None, Condition=None):
        global candidate_db

        if Condition is not None:
            condition           = Condition
            condition_negated_f = False
        else:
            condition, condition_negated_f = self.__condition(Condition_ComputedGoto)

        # Name --> Type(0), InitialValue(1), PriorityF(2)
        x = candidate_db[Name]

        if Index is not None:   Name = Name % Index

        if Type    is None: Type    = x[0]
        if Initial is None: Initial = x[1]

        self.__enter(Name, Type, None, Initial, condition, condition_negated_f, x[2])

        return Name

    def require_array(self, Name, ElementN, Initial, Index=None, Condition_ComputedGoto=None):
        global candidate_db
        IndexOrTuple = Index

        condition, condition_negated_f = self.__condition(Condition_ComputedGoto)

        # Name --> Type(0), InitialValue(1), PriorityF(2)
        x = candidate_db[Name]

        if IndexOrTuple is not None: Name = Name % IndexOrTuple
        self.__enter(Name, x[0], ElementN, Initial, condition, condition_negated_f, x[2])

        return Name

    def __str__(self):
        if len(self.__db) == 0:
            return ""
        L = max([len(x) for x in self.__db.iterkeys()])
        txt = []
        for name, info in self.__db.iteritems():
            txt.append("%s%s %s" % (name, " " * (L - len(name)), str(info)))
        return "".join(txt)

variable_db = VariableDB()
