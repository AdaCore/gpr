
from   quex.input.setup                                import NotificationDB
import quex.input.regular_expression.core              as     regular_expression
from   quex.input.regular_expression.construct         import Pattern           
import quex.input.files.mode_option                    as     mode_option
from   quex.input.files.mode_option                    import OptionDB
import quex.input.files.code_fragment                  as     code_fragment
from   quex.input.files.consistency_check              import __error_message as c_error_message
import quex.input.files.parser_data.patterns_and_terminals as patterns_and_terminals
from   quex.input.code.core                            import CodeUser
from   quex.input.code.base                            import SourceRef

import quex.engine.state_machine.check.same            as     same_check
import quex.engine.state_machine.check.outrun          as     outrun_checker
import quex.engine.state_machine.check.superset        as     superset_check
import quex.engine.misc.error                          as     error
from   quex.engine.misc.file_in                        import EndOfStreamException, \
                                                              check, \
                                                              check_or_die, \
                                                              read_identifier, \
                                                              read_until_letter, \
                                                              read_until_whitespace, \
                                                              skip_whitespace
from   quex.engine.incidence_db                        import IncidenceDB
from   quex.engine.misc.tools import typed, all_isinstance
import quex.blackboard as blackboard
from   quex.blackboard import setup as Setup, \
                              standard_incidence_db, \
                              E_IncidenceIDs

from   copy        import deepcopy
from   collections import namedtuple
from   itertools   import islice

#-----------------------------------------------------------------------------------------
# ModeDescription/Mode Objects:
#
# During parsing 'ModeDescription' objects are generated. Once parsing is over, 
# the descriptions are translated into 'real' mode objects where code can be generated
# from. All matters of inheritance and pattern resolution are handled in the
# transition from description to real mode.
#-----------------------------------------------------------------------------------------

PatternRepriorization = namedtuple("PatternRepriorization", ("pattern", "new_pattern_index", "sr"))
PatternDeletion       = namedtuple("PatternDeletion",       ("pattern", "pattern_index",     "sr"))

class PatternActionInfo(object):
    __slots__ = ("__pattern", "__action")
    def __init__(self, ThePattern, TheAction):
        self.__pattern = ThePattern
        self.__action  = TheAction

    @property
    def line_n(self):    return self.action().sr.line_n
    @property
    def file_name(self): return self.action().sr.file_name

    def pattern(self):   return self.__pattern

    def action(self):    return self.__action

    def __repr__(self):         
        txt  = ""
        if self.pattern() not in E_IncidenceIDs:
            txt += "self.pattern_string = %s\n" % repr(self.pattern().pattern_string())
        txt += "self.pattern        = %s\n" % repr(self.pattern()).replace("\n", "\n      ")
        txt += "self.action         = %s\n" % self.action().get_text()
        if hasattr(self.action(), "sr"):
            txt += "self.file_name  = %s\n" % repr(self.action().sr.file_name) 
            txt += "self.line_n     = %s\n" % repr(self.action().sr.line_n) 
        txt += "self.incidence_id = %s\n" % repr(self.pattern().incidence_id()) 
        return txt

class ModeDescription:
    """Mode description delivered directly from the parser.
    ______________________________________________________________________________
    MAIN MEMBERS:

     (1) .pattern_action_pair_list:   [ (Pattern, CodeUser) ]

     Lists all patterns which are directly defined in this mode (not the ones
     from the base mode) together with the user code (class CodeUser) to be
     executed upon the detected match.

     (2) .incidence_db:               incidence_id --> [ CodeFragment ]

     Lists of possible incidences (e.g. 'on_match', 'on_enter', ...) together
     with the code fragment to be executed upon occurence.

     (3) .option_db:                  option name  --> [ OptionSetting ]

     Maps the name of a mode option to a list of OptionSetting according to 
     what has been defined in the mode. Those options describe

        -- [optional] The character counter behavior.
        -- [optional] The indentation handler behavior.
        -- [optional] The 'skip' character behavior.
           ...

     That is, they parameterize code generators of 'helpers'. The option_db
     also contains information about mode transition restriction, inheritance
     behavior, etc.

     (*) .derived_from_list:   [ string ]
     
     Lists all modes from where this mode is derived, that is only the direct 
     super modes.
    ______________________________________________________________________________
    OTHER NON_TRIVIAL MEMBERS:

     If the mode is derived from another mode, it may make sense to adapt the 
     priority of patterns and/or delete pattern from the matching hierarchy.

     (*) .reprioritization_info_list: [ PatternRepriorization ] 
       
     (*) .deletion_info_list:         [ PatternDeletion ] 
    ______________________________________________________________________________
    """
    __slots__ = ("name",
                 "sr",
                 "derived_from_list",
                 "option_db",
                 "pattern_action_pair_list",
                 "incidence_db",
                 "reprioritization_info_list",
                 "deletion_info_list")

    def __init__(self, Name, SourceReference):
        # Register ModeDescription at the mode database
        blackboard.mode_description_db[Name] = self
        self.name  = Name
        self.sr    = SourceReference

        self.derived_from_list          = []

        self.pattern_action_pair_list   = []  
        self.option_db                  = OptionDB()    # map: option_name    --> OptionSetting
        self.incidence_db               = IncidenceDB() # map: incidence_name --> CodeFragment

        self.reprioritization_info_list = []  
        self.deletion_info_list         = [] 

    @typed(ThePattern=Pattern, Action=CodeUser)
    def add_pattern_action_pair(self, ThePattern, TheAction, fh):
        assert ThePattern.check_consistency()

        if ThePattern.pre_context_trivial_begin_of_line_f:
            blackboard.required_support_begin_of_line_set()

        TheAction.set_source_reference(SourceRef.from_FileHandle(fh, self.name))

        self.pattern_action_pair_list.append(PatternActionInfo(ThePattern, TheAction))

    def add_match_priority(self, ThePattern, fh):
        """Whenever a pattern in the mode occurs, which is identical to that given
           by 'ThePattern', then the priority is adapted to the pattern index given
           by the current pattern index.
        """
        PatternIdx = ThePattern.incidence_id() 
        self.reprioritization_info_list.append(
            PatternRepriorization(ThePattern, PatternIdx, 
                                  SourceRef.from_FileHandle(fh, self.name))
        )

    def add_match_deletion(self, ThePattern, fh):
        """If one of the base modes contains a pattern which is identical to this
           pattern, it has to be deleted.
        """
        PatternIdx = ThePattern.incidence_id() 
        self.deletion_info_list.append(
            PatternDeletion(ThePattern, PatternIdx, 
                            SourceRef.from_FileHandle(fh, self.name))
        )

class Mode:
    """Finalized 'Mode' as it results from combination of base modes.
    ____________________________________________________________________________

     A pattern detection mode. It is identified by

       .pattern_list -- A list of patterns which can potentially be detected.
                        A pattern match is a special kind of an incidence.
                        Pattern matches are associated with pattern match
                        actions (i.e. CodeFragment-s).

       .incidence_db -- A mapping from incidence ids to CodeFragments to be
                        executed upon the occurrence of the incidence.
     
                        NOTE: The incidences mentioned in 'incidence_db' are
                        all 'terminals' and NOT things which appear 'by the side'.

     A Mode is built upon a ModeDescription object. A mode description contains
     further 'option_db' such as a column-line-count specification and a
     indentation setup.
    ____________________________________________________________________________
    """
    focus = ("<skip>", "<skip_range>", "<skip_nested_range>", "<indentation newline>")

    def __init__(self, Origin):
        """Translate a ModeDescription into a real Mode. Here is the place were 
        all rules of inheritance mechanisms and pattern precedence are applied.
        """
        assert isinstance(Origin, ModeDescription)
        self.name = Origin.name
        self.sr   = Origin.sr   # 'SourceRef' -- is immutable

        base_mode_sequence  = self.__determine_base_mode_sequence(Origin, [], [])
        # At least the mode itself must be there
        # The mode itself is base_mode_sequence[-1]
        assert len(base_mode_sequence) >= 1 \
               and base_mode_sequence[-1].name == self.name

        # Collect Options
        # (A finalized Mode does not contain an option_db anymore).
        options_db   = OptionDB.from_BaseModeSequence(base_mode_sequence)
        incidence_db = IncidenceDB.from_BaseModeSequence(base_mode_sequence)

        # Determine Line/Column Counter Database
        counter_db   = options_db.value("counter")

        # Intermediate Step: Priority-Pattern-Terminal List (PPT list)
        #
        # The list is developed so that patterns can be sorted and code 
        # fragments are prepared.
        self.__pattern_list, \
        self.__terminal_db,  \
        self.__default_character_counter_required_f, \
        self.__doc_history_deletion, \
        self.__doc_history_reprioritization = \
                                      patterns_and_terminals.get(base_mode_sequence, 
                                                                 options_db, 
                                                                 counter_db, 
                                                                 incidence_db)
        
        # (*) Misc
        self.__abstract_f           = self.__is_abstract(Origin.incidence_db, 
                                                         Origin.option_db)
        self.__base_mode_sequence   = base_mode_sequence
        self.__entry_mode_name_list = options_db.value_list("entry") # Those can enter this mode.
        self.__exit_mode_name_list  = options_db.value_list("exit")  # This mode can exit to those.
        self.__incidence_db         = incidence_db
        self.__counter_db           = counter_db
        self.__on_after_match_code  = incidence_db.get(E_IncidenceIDs.AFTER_MATCH)
        self.__indentation_setup    = options_db.value("indentation")

    def abstract_f(self):           return self.__abstract_f

    @property
    def counter_db(self):           return self.__counter_db

    @property
    def exit_mode_name_list(self):  return self.__exit_mode_name_list

    @property
    def entry_mode_name_list(self): return self.__entry_mode_name_list

    @property
    def incidence_db(self): return self.__incidence_db

    @property
    def pattern_list(self):        return self.__pattern_list
    @property
    def on_after_match_code(self): return self.__on_after_match_code
    @property
    def terminal_db(self):         return self.__terminal_db

    @property
    def default_character_counter_required_f(self): return self.__default_character_counter_required_f

    def has_base_mode(self):
        return len(self.__base_mode_sequence) != 1

    def get_base_mode_sequence(self):
        assert len(self.__base_mode_sequence) >= 1 # At least the mode itself is in there
        return self.__base_mode_sequence

    def get_base_mode_name_list(self):
        assert len(self.__base_mode_sequence) >= 1 # At least the mode itself is in there
        return [ mode.name for mode in self.__base_mode_sequence ]

    def __is_abstract(self, OriginalIncidenceDb, OriginalOptionDb):
        """If the mode has incidences and/or patterns defined it is free to be 
        abstract or not. If neither one is defined, it cannot be implemented and 
        therefore MUST be abstract.
        """
        abstract_f = (OriginalOptionDb.value("inheritable") == "only")

        if len(OriginalIncidenceDb) != 0 or len(self.pattern_list) != 0:
            return abstract_f

        elif abstract_f == False:
            error.warning("Mode without pattern and event handlers needs to be 'inheritable only'.\n" + \
                          "<inheritable: only> has been set automatically.", self.sr)
            abstract_f = True # Change to 'inheritable: only', i.e. abstract_f == True.

        return abstract_f

    def is_implemented(self):
        return not(self.abstract_f() or len(self.pattern_list) == 0)

    def __determine_base_mode_sequence(self, ModeDescr, InheritancePath, base_mode_sequence):
        """Determine the sequence of base modes. The type of sequencing determines
           also the pattern precedence. The 'deep first' scheme is chosen here. For
           example a mode hierarchie of

                                       A
                                     /   \ 
                                    B     C
                                   / \   / \
                                  D  E  F   G

           results in a sequence: (A, B, D, E, C, F, G).reverse()

           => That is the mode itself is base_mode_sequence[-1]

           => Patterns and event handlers of 'E' have precedence over
              'C' because they are the childs of a preceding base mode.

           This function detects circular inheritance.

        __dive -- inserted this keyword for the sole purpose to signal 
                  that here is a case of recursion, which may be solved
                  later on by a TreeWalker.
        """
        if ModeDescr.name in InheritancePath:
            msg = "mode '%s'\n" % InheritancePath[0]
            for mode_name in InheritancePath[InheritancePath.index(ModeDescr.name) + 1:]:
                msg += "   inherits mode '%s'\n" % mode_name
            msg += "   inherits mode '%s'" % ModeDescr.name

            error.log("circular inheritance detected:\n" + msg, ModeDescr.sr)

        base_mode_name_list_reversed = deepcopy(ModeDescr.derived_from_list)
        #base_mode_name_list_reversed.reverse()
        for name in base_mode_name_list_reversed:
            # -- does mode exist?
            error.verify_word_in_list(name, blackboard.mode_description_db.keys(),
                                      "Mode '%s' inherits mode '%s' which does not exist." % (ModeDescr.name, name),
                                      ModeDescr.sr)

            if name in map(lambda m: m.name, base_mode_sequence): continue

            # -- grab the mode description
            mode_descr = blackboard.mode_description_db[name]
            self.__determine_base_mode_sequence(mode_descr, InheritancePath + [ModeDescr.name], base_mode_sequence)

        base_mode_sequence.append(ModeDescr)

        return base_mode_sequence

    def check_consistency(self):
        # (*) Modes that are inherited must allow to be inherited
        #     __base_mode_sequence[-1] == the mode itself.
        for base_mode in self.__base_mode_sequence[:-1]:
            if base_mode.option_db.value("inheritable") == "no":
                error.log("mode '%s' inherits mode '%s' which is not inheritable." % \
                          (self.name, base_mode.name), self.sr)

        # (*) Empty modes which are not inheritable only?
        # (*) A mode that is instantiable (to be implemented) needs finally contain matches!
        if (not self.__abstract_f)  and len(self.__pattern_list) == 0:
            error.log("Mode '%s' was defined without the option <inheritable: only>.\n" % self.name + \
                      "However, it contains no matches--only event handlers. Without pattern\n"     + \
                      "matches it cannot act as a pattern detecting state machine, and thus\n"      + \
                      "cannot be an independent lexical analyzer mode. Define the option\n"         + \
                      "<inheritable: only>.", \
                      self.sr)

    def unique_pattern_pair_iterable(self):
        """Iterates over pairs of patterns:

            (high precedence pattern, low precedence pattern)

           where 'pattern_i' as precedence over 'pattern_k'
        """
        for i, high in enumerate(self.__pattern_list):
            for low in islice(self.__pattern_list, i+1, None):
                yield high, low

    def check_special_incidence_outrun(self, ErrorCode):
        for high, low in self.unique_pattern_pair_iterable():
            if     high.pattern_string() not in Mode.focus \
               and low.pattern_string()  not in Mode.focus: continue
            
            elif not outrun_checker.do(high.sm, low.sm):                  
                continue
            c_error_message(high, low, ExitF=True, 
                            ThisComment  = "has lower priority but",
                            ThatComment  = "may outrun",
                            SuppressCode = ErrorCode)
                                 
    def check_higher_priority_matches_subset(self, ErrorCode):
        """Checks whether a higher prioritized pattern matches a common subset
           of the ReferenceSM. For special patterns of skipper, etc. this would
           be highly confusing.
        """
        global special_pattern_list
        for high, low in self.unique_pattern_pair_iterable():
            if     high.pattern_string() not in Mode.focus \
               and low.pattern_string() not in Mode.focus: continue

            if not superset_check.do(high.sm, low.sm):             
                continue

            c_error_message(high, low, ExitF=True, 
                            ThisComment  = "has higher priority and",
                            ThatComment  = "matches a subset of",
                            SuppressCode = ErrorCode)

    def check_dominated_pattern(self, ErrorCode):
        for high, low in self.unique_pattern_pair_iterable():
            # 'low' comes after 'high' => 'i' has precedence
            # Check for domination.
            if superset_check.do(high, low):
                c_error_message(high, low, 
                                ThisComment  = "matches a superset of what is matched by",
                                EndComment   = "The former has precedence and the latter can never match.",
                                ExitF        = True, 
                                SuppressCode = ErrorCode)

    def check_match_same(self, ErrorCode):
        """Special patterns shall never match on some common lexemes."""
        for high, low in self.unique_pattern_pair_iterable():
            if     high.pattern_string() not in Mode.focus \
               and low.pattern_string() not in Mode.focus: continue

            # A superset of B, or B superset of A => there are common matches.
            if not same_check.do(high.sm, low.sm): continue

            # The 'match what remains' is exempted from check.
            if high.pattern_string() == "." or low.pattern_string() == ".":
                continue

            c_error_message(high, low, 
                            ThisComment  = "matches on some common lexemes as",
                            ThatComment  = "",
                            ExitF        = True,
                            SuppressCode = ErrorCode)

    def check_low_priority_outruns_high_priority_pattern(self):
        """Warn when low priority patterns may outrun high priority patterns.
        Assume that the pattern list is sorted by priority!
        """
        for high, low in self.unique_pattern_pair_iterable():
            if outrun_checker.do(high.sm, low.sm):
                c_error_message(low, high, ExitF=False, ThisComment="may outrun")

    def get_documentation(self):
        L = max(map(lambda mode: len(mode.name), self.__base_mode_sequence))
        txt  = "\nMODE: %s\n" % self.name

        txt += "\n"
        if len(self.__base_mode_sequence) != 1:
            txt += "    BASE MODE SEQUENCE:\n"
            base_mode_name_list = map(lambda mode: mode.name, self.__base_mode_sequence[:-1])
            base_mode_name_list.reverse()
            for name in base_mode_name_list:
                txt += "      %s\n" % name
            txt += "\n"

        if len(self.__doc_history_deletion) != 0:
            txt += "    DELETION ACTIONS:\n"
            for entry in self.__doc_history_deletion:
                txt += "      %s:  %s%s  (from mode %s)\n" % \
                       (entry[0], " " * (L - len(self.name)), entry[1], entry[2])
            txt += "\n"

        if len(self.__doc_history_reprioritization) != 0:
            txt += "    PRIORITY-MARK ACTIONS:\n"
            self.__doc_history_reprioritization.sort(lambda x, y: cmp(x[4], y[4]))
            for entry in self.__doc_history_reprioritization:
                txt += "      %s: %s%s  (from mode %s)  (%i) --> (%i)\n" % \
                       (entry[0], " " * (L - len(self.name)), entry[1], entry[2], entry[3], entry[4])
            txt += "\n"

        assert all_isinstance(self.__pattern_list, Pattern)
        if len(self.__pattern_list) != 0:
            txt += "    PATTERN LIST:\n"
            for x in self.__pattern_list:
                space  = " " * (L - len(x.sr.mode_name)) 
                txt   += "      (%3i) %s: %s%s\n" % \
                         (x.incidence_id(), x.sr.mode_name, space, x.pattern_string())
            txt += "\n"

        return txt

def parse(fh):
    """This function parses a mode description and enters it into the 
       'blackboard.mode_description_db'. Once all modes are parsed
       they can be translated into 'real' modes and are located in
       'blackboard.mode_db'. 
    """

    # NOTE: Catching of EOF happens in caller: parse_section(...)
    skip_whitespace(fh)
    mode_name = read_identifier(fh, OnMissingStr="Missing identifier at beginning of mode definition.")

    # NOTE: constructor does register this mode in the mode_db
    new_mode  = ModeDescription(mode_name, SourceRef.from_FileHandle(fh))

    # (*) inherited modes / option_db
    skip_whitespace(fh)
    dummy = fh.read(1)
    if dummy not in [":", "{"]:
        error.log("missing ':' or '{' after mode '%s'" % mode_name, fh)

    if dummy == ":":
        __parse_option_list(new_mode, fh)

    # (*) read in pattern-action pairs and events
    while __parse_element(new_mode, fh): 
        pass

def determine_start_mode(mode_db):
    if not blackboard.initial_mode.sr.is_void():
        return

    # Choose an applicable mode as start mode
    first_candidate = None
    for name, mode in mode_db.iteritems():
        if mode.abstract_f(): 
            continue
        elif first_candidate is not None:
            error.log("No initial mode defined via 'start' while more than one applicable mode exists.\n" + \
                      "Use for example 'start = %s;' in the quex source file to define an initial mode." \
                      % first_candidate.name)
        else:
            first_candidate = mode

    if first_candidate is None:
        error.log("No mode that can be implemented--all modes <inheritable: only>.")
    else:
        blackboard.initial_mode = CodeUser(first_candidate.name, SourceReference=first_candidate.sr)

def __parse_option_list(new_mode, fh):
    position = fh.tell()
    try:  
        # ':' => inherited modes/option_db follow
        skip_whitespace(fh)

        __parse_base_mode_list(fh, new_mode)
        
        while mode_option.parse(fh, new_mode):
            pass

    except EndOfStreamException:
        fh.seek(position)
        error.error_eof("mode '%s'." % new_mode.name, fh)

def __parse_base_mode_list(fh, new_mode):
    new_mode.derived_from_list = []
    trailing_comma_f    = False
    while 1 + 1 == 2:
        if   check(fh, "{"): fh.seek(-1, 1); break
        elif check(fh, "<"): fh.seek(-1, 1); break

        skip_whitespace(fh)
        identifier = read_identifier(fh)
        if identifier == "": break

        new_mode.derived_from_list.append(identifier)
        trailing_comma_f = False
        if not check(fh, ","): break
        trailing_comma_f = True


    if trailing_comma_f:
        error.warning("Trailing ',' after base mode '%s'." % new_mode.derived_from_list[-1], fh) 
        
    elif len(new_mode.derived_from_list) != 0:
        # This check is a 'service' -- for those who follow the old convention
        pos = fh.tell()
        skip_whitespace(fh)
        dummy_identifier = read_identifier(fh)
        if dummy_identifier != "":
            error.log("Missing separating ',' between base modes '%s' and '%s'.\n" \
                      % (new_mode.derived_from_list[-1], dummy_identifier) + \
                      "(The comma separator is mandatory since quex 0.53.1)", fh)
        fh.seek(pos)

def __parse_element(new_mode, fh):
    """Returns: False, if a closing '}' has been found.
                True, else.
    """
    position = fh.tell()
    try:
        description = "pattern or event handler" 

        skip_whitespace(fh)
        # NOTE: Do not use 'read_word' since we need to continue directly after
        #       whitespace, if a regular expression is to be parsed.
        position = fh.tell()

        word = read_until_whitespace(fh)
        if word == "}": return False

        # -- check for 'on_entry', 'on_exit', ...
        if __parse_event(new_mode, fh, word): return True

        fh.seek(position)
        description = "start of mode element: regular expression"
        pattern     = regular_expression.parse(fh)
        pattern.set_source_reference(SourceRef.from_FileHandle(fh, new_mode.name))

        position    = fh.tell()
        description = "start of mode element: code fragment for '%s'" % pattern.pattern_string()

        __parse_action(new_mode, fh, pattern.pattern_string(), pattern)

    except EndOfStreamException:
        fh.seek(position)
        error.error_eof(description, fh)

    return True

def __parse_action(new_mode, fh, pattern_str, pattern):

    position = fh.tell()
    try:
        skip_whitespace(fh)
        position = fh.tell()
            
        code = code_fragment.parse(fh, "regular expression", ErrorOnFailureF=False) 
        if code is not None:
            assert isinstance(code, CodeUser), "Found: %s" % code.__class__
            new_mode.add_pattern_action_pair(pattern, code, fh)
            return

        fh.seek(position)
        word = read_until_letter(fh, [";"])
        if word == "PRIORITY-MARK":
            # This mark 'lowers' the priority of a pattern to the priority of the current
            # pattern index (important for inherited patterns, that have higher precedence).
            # The parser already constructed a state machine for the pattern that is to
            # be assigned a new priority. Since, this machine is not used, let us just
            # use its id.
            fh.seek(-1, 1)
            check_or_die(fh, ";", ". Since quex version 0.33.5 this is required.")
            new_mode.add_match_priority(pattern, fh)

        elif word == "DELETION":
            # This mark deletes any pattern that was inherited with the same 'name'
            fh.seek(-1, 1)
            check_or_die(fh, ";", ". Since quex version 0.33.5 this is required.")
            new_mode.add_match_deletion(pattern, fh)
            
        else:
            error.log("Missing token '{', 'PRIORITY-MARK', 'DELETION', or '=>' after '%s'.\n" % pattern_str + \
                      "found: '%s'. Note, that since quex version 0.33.5 it is required to add a ';'\n" % word + \
                      "to the commands PRIORITY-MARK and DELETION.", fh)


    except EndOfStreamException:
        fh.seek(position)
        error.error_eof("pattern action", fh)

def __parse_event(new_mode, fh, word):
    pos = fh.tell()

    # Allow '<<EOF>>' and '<<FAIL>>' out of respect for classical tools like 'lex'
    if   word == "<<EOF>>":                  word = "on_end_of_stream"
    elif word == "<<FAIL>>":                 word = "on_failure"
    elif word in blackboard.all_section_title_list:
        error.log("Pattern '%s' is a quex section title. Has the closing '}' of mode %s \n" % (word, new_mode.name) \
                  + "been forgotten? Else use quotes, i.e. \"%s\"." % word, fh)
    elif len(word) < 3 or word[:3] != "on_": return False

    comment = "Unknown event handler '%s'. \n" % word + \
              "Note, that any pattern starting with 'on_' is considered an event handler.\n" + \
              "use double quotes to bracket patterns that start with 'on_'."

    __general_validate(fh, new_mode, word, pos)
    error.verify_word_in_list(word, standard_incidence_db.keys(), comment, 
                              fh)
    __validate_required_token_policy_queue(word, fh, pos)

    continue_f = True
    if word == "on_end_of_stream" or word == "on_failure":
        # -- When a termination token is sent, no other token shall follow. 
        #    => Enforce return from the analyzer! Do not allow CONTINUE!
        # -- When an 'on_failure' is received allow immediate action of the
        #    receiver => Do not allow CONTINUE!
        continue_f = False

    new_mode.incidence_db[word] = \
            code_fragment.parse(fh, "%s::%s event handler" % (new_mode.name, word),
                                ContinueF=continue_f)

    return True

def __general_validate(fh, Mode, Name, pos):
    if Name == "on_indentation":
        fh.seek(pos)
        error.log("Definition of 'on_indentation' is no longer supported since version 0.51.1.\n"
                  "Please, use 'on_indent' for the event of an opening indentation, 'on_dedent'\n"
                  "for closing indentation, and 'on_nodent' for no change in indentation.", fh) 


    def error_dedent_and_ndedent(code, A, B):
        error.log("Indentation event handler '%s' cannot be defined, because\n" % A,
                  fh, DontExitF=True)
        error.log("the alternative '%s' has already been defined." % B,
                  code.sr)

    if Name == "on_dedent" and Mode.incidence_db.has_key("on_n_dedent"):
        fh.seek(pos)
        code = Mode.incidence_db["on_n_dedent"]
        if not code.is_whitespace():
            error_dedent_and_ndedent(code, "on_dedent", "on_n_dedent")
                      
    if Name == "on_n_dedent" and Mode.incidence_db.has_key("on_dedent"):
        fh.seek(pos)
        code = Mode.incidence_db["on_dedent"]
        if not code.is_whitespace():
            error_dedent_and_ndedent(code, "on_n_dedent", "on_dedent")
                      
def __validate_required_token_policy_queue(Name, fh, pos):
    """Some handlers are better only used with token policy 'queue'."""

    if Name not in ["on_entry", "on_exit", 
                    "on_indent", "on_n_dedent", "on_dedent", "on_nodent", 
                    "on_indentation_bad", "on_indentation_error", 
                    "on_indentation"]: 
        return
    if Setup.token_policy == "queue":
        return

    pos_before = fh.tell()
    fh.seek(pos)
    error.warning("Using '%s' event handler, while the token queue is disabled.\n" % Name + \
                  "Use '--token-policy queue', so then tokens can be sent safer\n" + \
                  "from inside this event handler.", fh,
                  SuppressCode=NotificationDB.warning_on_no_token_queue) 
    fh.seek(pos_before)

