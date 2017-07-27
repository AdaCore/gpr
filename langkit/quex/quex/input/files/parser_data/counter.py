# (C) Frank-Rene Schaefer
from   quex.input.setup                           import NotificationDB
from   quex.engine.misc.tools                     import typed
import quex.engine.misc.error                     as     error
from   quex.engine.state_machine.core             import StateMachine  
import quex.engine.state_machine.algorithm.beautifier as beautifier    
from   quex.input.code.base            import SourceRefObject, \
                                                         SourceRef, \
                                                         SourceRef_DEFAULT
from   quex.engine.misc.interval_handling              import NumberSet
from   quex.blackboard                            import E_CharacterCountType

from   quex.blackboard import setup as Setup
from   collections     import namedtuple, defaultdict
from   operator        import itemgetter

cc_type_db = {
    "space":                     E_CharacterCountType.COLUMN,
    "grid":                      E_CharacterCountType.GRID,
    "newline":                   E_CharacterCountType.LINE,
    "begin(newline suppressor)": E_CharacterCountType.BEGIN_NEWLINE_SUPPRESSOR,
    "begin(newline)":            E_CharacterCountType.BEGIN_NEWLINE,
    "begin(comment to newline)": E_CharacterCountType.BEGIN_COMMENT_TO_NEWLINE,
    "end(newline)":              E_CharacterCountType.END_NEWLINE,
    "bad":                       E_CharacterCountType.BAD,
    "whitespace":                E_CharacterCountType.WHITESPACE,
}

cc_type_name_db = dict((value, key) for key, value in cc_type_db.iteritems())


class CountOpMapEntry(namedtuple("CountOpMapEntry", ("cc_type", "value", "sr"))):
    def __new__(self, CCType, Value, sr):
        return super(CountOpMapEntry, self).__new__(self, CCType, Value, sr)

CountInfo = namedtuple("CountInfo",        ("incidence_id", "cc_type", "parameter", "character_set"))

class CountOpMap(object):
    """Association of character sets with triggered count commands.
    ___________________________________________________________________________

                   list: (character set, CountOpMapEntry)

    where the 'character set' specifies a subset of characters for which there
    is a definition by the given 'parameter'. The character sets are disjoint.

    This map is used to determine whether actions on character sets are defined 
    more than once. The CountOpMapEntry contains source references. This allows
    for detailed error messages.
    ___________________________________________________________________________
    """
    __slots__ = ("__map", "__else")
    def __init__(self):
        """Primarily, the '__map' member stores the list of associations between
        character sets and the count command entry. The '__else' contains the 
        count command which waits to be applied to the remaining set of characters.
        """
        self.__map  = []
        self.__else = None

    def get_map(self):
        return self.__map

    def define_else(self, Identifier, Value, sr):
        """Define the '\else' character set which is resolved AFTER everything has been 
        defined.
        """
        global cc_type_db

        if self.__else is not None:
            error.log("'\\else has been defined more than once.", sr, 
                      DontExitF=True)
            error.log("Previously, defined here.", self.__else.sr)
        self.__else = CountOpMapEntry(cc_type_db[Identifier], Value, sr)

    def add(self, CharSet, Identifier, Value, sr):
        global cc_type_db
        if CharSet.is_empty(): 
            error.log("Empty character set found for '%s'." % Identifier, sr)
        elif Identifier == "grid":
            self.check_grid_specification(Value, sr)
        cc_type = cc_type_db[Identifier]
        self.check_intersection(cc_type, CharSet, sr)
        self.__map.append((CharSet, CountOpMapEntry(cc_type, Value, sr)))

    def get_count_commands(self, CharacterSet):
        """Finds the count command for column, grid, and newline. This does NOT
        consider 'chunk number per character'. The consideration is on pure 
        character (unicode) level.
        
        RETURNS: [0] column increment (None, if none, -1 if undetermined)
                 [1] grid step size   (None, if none, -1 if undetermined)
                 [2] line increment   (None, if none, -1 if undetermined)

            None --> no influence from CharacterSet on setting.
            '-1' --> no distinct influence from CharacterSet on setting.
                     (more than one possible).

        NOTE: If one value not in (None, -1), then all others must be None.
        """

        db = {
            E_CharacterCountType.COLUMN: None,
            E_CharacterCountType.GRID:   None,
            E_CharacterCountType.LINE:   None,
        }

        for character_set, entry in self.__map:
            if entry.cc_type not in db: 
                continue
            elif character_set.is_superset(CharacterSet):
                db[entry.cc_type] = entry.value
                break
            elif character_set.has_intersection(CharacterSet): 
                db[entry.cc_type] = -1     

        return db[E_CharacterCountType.COLUMN], \
               db[E_CharacterCountType.GRID], \
               db[E_CharacterCountType.LINE]

    def check_intersection(self, CcType, CharSet, sr):
        """Check whether the given character set 'CharSet' intersects with 
        a character set already mentioned in the map. Depending on the CcType
        of the new candidate certain count commands may be tolerated, i.e. 
        their intersection is not considered.
        """
        intersection_tolerated = {
            E_CharacterCountType.COLUMN:                   (),
            E_CharacterCountType.GRID:                     (),
            E_CharacterCountType.LINE:                     (),
            E_CharacterCountType.BEGIN_NEWLINE_SUPPRESSOR: (),
            E_CharacterCountType.BEGIN_NEWLINE:            (E_CharacterCountType.END_NEWLINE,),
            E_CharacterCountType.END_NEWLINE:              (E_CharacterCountType.BEGIN_NEWLINE,),
            E_CharacterCountType.BEGIN_COMMENT_TO_NEWLINE: (), 
            E_CharacterCountType.WHITESPACE:               (),
            E_CharacterCountType.BAD:                      (), 
        }[CcType]

        interferer = self.find_occupier(CharSet, Tolerated=intersection_tolerated)
        if interferer is None:
            return
        _error_set_intersection(CcType, interferer, sr)

    def get_remaining_set(self, GlobalMin, GlobalMax):
        """Return the set of characters which are not associated with count commands.
        Restrict the operation to characters from GlobalMin to GlobalMax (inclusively).
        """
        result = self.__get_remaining_set()
        result.cut_lesser(GlobalMin)
        result.cut_greater_or_equal(GlobalMax)
        return result

    def assign_else_count_command(self, GlobalMin, GlobalMax, SourceReference):
        """After all count commands have been assigned to characters, the 
        remaining character set can be associated with the 'else-CountOpMapEntry'.
        """
        if self.__else is None: 
            else_cmd = CountOpMapEntry(E_CharacterCountType.COLUMN, 1, SourceRef_DEFAULT)
            error.warning("No '\else' defined in counter setup. Assume '\else => space 1;'", SourceReference, 
                          SuppressCode=NotificationDB.warning_counter_setup_without_else)
        else:                   
            else_cmd = self.__else
        
        remaining_set = self.get_remaining_set(GlobalMin, GlobalMax)
        if not remaining_set.is_empty():
            self.__map.append((remaining_set, else_cmd))

    def column_grid_line_iterable_pruned(self, CharacterSet):
        """Iterate over count command map. It is assumed that anything in the map
        is 'valid'. 
        """
        considered_set = (E_CharacterCountType.COLUMN, 
                          E_CharacterCountType.GRID, 
                          E_CharacterCountType.LINE)
        for character_set, info in self.__map:
            if character_set.has_intersection(CharacterSet):
                if info.cc_type not in considered_set: continue
                yield character_set.intersection(CharacterSet), info

    def column_grid_bad_iterable(self):
        """Iterate over count command map. Only 'COLUMN' and 'GRID' are reported. 
        This is for indentation counting.
        """
        considered_set = (E_CharacterCountType.COLUMN, 
                          E_CharacterCountType.GRID, 
                          E_CharacterCountType.BAD)
        for character_set, info in self.__map:
            if info.cc_type in considered_set:
                yield character_set, info

    @typed(CharacterSet=NumberSet)
    def get_column_number_per_chunk(self, CharacterSet):
        """Considers the counter database which tells what character causes
        what increment in line and column numbers. However, only those characters
        are considered which appear in the CharacterSet. 

        RETURNS: None -- If there is NO distinct column increment.
                 >= 0 -- The increment of column number for every character
                         from CharacterSet.
        """
        column_incr_per_character = None
        number_set                = None
        for character_set, info in self.column_grid_line_iterable_pruned(CharacterSet):
            if info.cc_type != E_CharacterCountType.COLUMN: 
                continue
            elif column_incr_per_character is None:       
                column_incr_per_character = info.value
                number_set                = character_set
            elif column_incr_per_character == info.value: 
                number_set.unite_with(character_set)
            else:
                return None

        if column_incr_per_character is None:
            return None                       # TODO: return 0

        # HERE: There is only ONE 'column_n_increment' command. It appears on
        # the character set 'number_set'. If the character set is represented
        # by the same number of chunks, than the column number per chunk is
        # found.
        if not Setup.buffer_codec.variable_character_sizes_f():
            return column_incr_per_character

        chunk_n_per_character = \
            Setup.buffer_codec.homogeneous_chunk_n_per_character(number_set) 
        if chunk_n_per_character is None:
            return None
        else:
            return float(column_incr_per_character) / chunk_n_per_character

    def find_occupier(self, CharSet, Tolerated):
        """Find a command that occupies the given CharSet, at least partly.
           RETURN: None, if no such occupier exists.
        """
        for character_set, before in self.__map:
            if   before.cc_type in Tolerated:                 continue
            elif not character_set.has_intersection(CharSet): continue
            return before
        return None

    def __get_remaining_set(self):
        ignored = (E_CharacterCountType.BAD, 
                   E_CharacterCountType.BEGIN_NEWLINE_SUPPRESSOR, 
                   E_CharacterCountType.BEGIN_NEWLINE, 
                   E_CharacterCountType.END_NEWLINE) 
        result  = NumberSet()
        for character_set, info in self.__map:
            if info.cc_type in ignored: continue
            result.unite_with(character_set)
        return result.get_complement(Setup.buffer_codec.source_set)

    def check_grid_values_integer_multiples(self):
        """If there are no spaces and the grid is on a homogeneous scale,
           => then the grid can be transformed into 'easy-to-compute' spaces.
        """
        grid_value_list = []
        min_info        = None
        for character_set, info in self.__map:
            if info.cc_type != E_CharacterCountType.GRID: 
                if info.cc_type == E_CharacterCountType.COLUMN: 
                    return
                continue
            elif type(info.value) in (str, unicode): 
                # If there is one single 'variable' grid value, 
                # then no assumptions can be made.
                return
            grid_value_list.append(info.value)
            if min_info is None or info.value < min_info.value:
                min_info = info

        if min_info is None:
            return

        # Are all grid values a multiple of the minimum?
        if len(filter(lambda x: x % min_info.value == 0, grid_value_list)) != len(grid_value_list):
            return

        error.warning("Setup does not contain spaces, only grids (tabulators). All grid\n" \
                      "widths are multiples of %i. The grid setup %s\n" \
                      % (min_info.value, repr(sorted(grid_value_list))[1:-1]) + \
                      "is equivalent to a setup with space counts %s.\n" \
                      % repr(map(lambda x: x / min_info.value, sorted(grid_value_list)))[1:-1] + \
                      "Space counts are faster to compute.", 
                      min_info.sr)

    def check_homogenous_space_counts(self):
        common = None
        for character_set, info in self.__map:
            if info.cc_type != E_CharacterCountType.COLUMN: 
                if info.cc_type == E_CharacterCountType.GRID: 
                    return
                continue
            elif type(info.value) in (str, unicode): 
                # If there is one single 'variable' grid value, 
                # then no assumptions can be made.
                return
            elif common is None:
                common = info
            elif common.value != info.value:
                # space counts are not homogeneous
                return

        if common is None:
            return
            
        error.warning("Setup does not contain a grid but only homogeneous space counts of %i.\n" \
                  % common.value + \
                  "This setup is equivalent to a setup with space counts of 1. Space counts\n" + \
                  "of 1 are the fastest to compute.", 
                  common.sr)

    def check_defined(self, SourceReference, CCT):
        """Checks whether the character counter type has been defined in the 
        map.
        
        THROWS: Error in case that is has not been defined.
        """
        for character_set, info in self.__map:
            if info.cc_type == CCT: 
                return

        error.warning("Setup does not define '%s'." % cc_type_name_db[CCT], SourceReference, 
                      SuppressCode=NotificationDB.warning_counter_setup_without_newline)

    def check_grid_specification(self, Value, sr):
        if   Value == 0: 
            error.log("A grid count of 0 is nonsense. May be define a space count of 0.", sr)
        elif Value == 1:
            error.warning("Indentation grid counts of '1' are equivalent of to a space\n" + \
                          "count of '1'. The latter is faster to compute.",
                              sr)

    def __str__(self):
        def _db_to_text(title, CountOpInfoList):
            txt = "%s:\n" % title
            for character_set, info in sorted(CountOpInfoList, key=lambda x: x[0].minimum()):
                if type(info.value) in [str, unicode]:
                    txt += "    %s by %s\n" % (info.value, character_set.get_utf8_string())
                else:
                    txt += "    %3i by %s\n" % (info.value, character_set.get_utf8_string())
            return txt

        db_by_name = defaultdict(list)
        for character_set, info in self.__map:
            name = cc_type_name_db[info.cc_type]
            db_by_name[name].append((character_set, info))

        txt = [
            _db_to_text(cname, count_command_info_list)
            for cname, count_command_info_list in sorted(db_by_name.iteritems(), key=itemgetter(0))
        ]
        return "".join(txt)

class Base:
    @typed(sr=SourceRef)
    def __init__(self, sr, Name, IdentifierList, TheCountOpMap=None):
        self.sr   = sr
        self.name = Name
        if TheCountOpMap is None: self.count_command_map = CountOpMap()
        else:                      self.count_command_map = TheCountOpMap
        self.identifier_list        = IdentifierList
        self.__containing_mode_name = ""

    @typed(sr=SourceRef, Identifier=(str,unicode))
    def specify(self, Identifier, Pattern, Count, sr):
        if Pattern is None:
            self.count_command_map.define_else(Identifier, Count, sr)
        else:
            self.count_command_map.add(extract_trigger_set(sr, Identifier, Pattern), 
                                       Identifier, Count, sr)

    def consistency_check(self):
        assert False, "Derived class must implement this."

    def set_containing_mode_name(self, ModeName):
        assert isinstance(ModeName, (str, unicode))
        self.__containing_mode_name = ModeName

    def containing_mode_name(self):
        return self.__containing_mode_name

class ParserDataLineColumn(Base):
    """Line/column number count specification.
    ___________________________________________________________________________
    The main result of the parsing the the Base's .count_command_map which is 
    an instance of CountOpMap.
    ____________________________________________________________________________
    """
    @typed(sr=SourceRef)
    def __init__(self, sr, TheCountOpMap=None):
        Base.__init__(self, sr, "Line/column counter", ("space", "grid", "newline"), TheCountOpMap)

    def finalize(self):
        # Assign the 'else' command to all the remaining places in the character map.
        self.count_command_map.assign_else_count_command(
                                 Setup.buffer_codec.source_set.minimum(), 
                                 Setup.buffer_codec.source_set.supremum(), 
                                 self.sr)
        self.consistency_check()

    def consistency_check(self):
        self.count_command_map.check_grid_values_integer_multiples()
        # The following is nonsense: We detect that we did not choose an alternative 
        # which is worse?!
        # self.count_command_map.check_homogenous_space_counts()
        self.count_command_map.check_defined(self.sr, E_CharacterCountType.LINE)

class ParserDataIndentation(Base):
    """Indentation counter specification.
    ____________________________________________________________________________
    The base's .count_command_map contains information about how to count the 
    space at the beginning of the line. The count until the first non-whitespace
    is the 'indentation'. 
    
    +bad:

    The spec contains information about what characters are not supposed to
    appear in indentation (bad characters). Depending on the philosophical
    basis, some might consider 'space' as evil, others consider 'tab' as evil.

    +newline:

    A detailed state machine can be defined for 'newline'. This might be 
    '\n|(\r\n)' or more complex things.

    +suppressor:

    A newline might be suppressed by '\' for example. For that, it might be
    specified as 'newline suppressor'.
    ____________________________________________________________________________
    """
    @typed(sr=SourceRef)
    def __init__(self, sr):
        self.whitespace_character_set = SourceRefObject("whitespace", None)
        self.bad_character_set        = SourceRefObject("bad", None)
        self.sm_newline               = SourceRefObject("newline", None)
        self.sm_newline_suppressor    = SourceRefObject("suppressor", None)
        self.sm_comment               = SourceRefObject("comment", None)

        Base.__init__(self, sr, "Indentation counter", ("whitespace", "comment", "newline", "suppressor", "bad"))

    def specify(self, identifier, pattern, sr):
        if   identifier == "whitespace": 
            self.__specify_character_set(self.whitespace_character_set, 
                                         "whitespace", pattern, sr)
        elif identifier == "bad":        
            self.__specify_character_set(self.bad_character_set, 
                                         "bad", pattern, sr)
        elif identifier == "newline":    
            self.specify_newline(pattern.sm, sr)
        elif identifier == "suppressor": 
            self.specify_suppressor(pattern.sm, sr)
        elif identifier == "comment":    
            self.specify_comment(pattern.sm, sr)
        else:                            
            return False
        return True

    @typed(sr=SourceRef)
    def __specify_character_set(self, ref, Name, PatternOrNumberSet, sr):
        cset = extract_trigger_set(sr, Name, PatternOrNumberSet)
        self.count_command_map.add(cset, Name, None, sr)
        prev_cset = ref.get()
        if prev_cset is None: ref.set(cset, sr)
        else:                 prev_cset.unite_with(cset)

    @typed(sr=SourceRef)
    def specify_newline(self, Sm, sr):
        _error_if_defined_before(self.sm_newline, sr)

        beginning_char_set = Sm.get_beginning_character_set()
        ending_char_set    = Sm.get_ending_character_set()

        self.count_command_map.add(beginning_char_set, "begin(newline)", None, sr)

        # Do not consider a character from newline twice
        ending_char_set.subtract(beginning_char_set)
        if not ending_char_set.is_empty():
            self.count_command_map.add(ending_char_set, "end(newline)", None, sr)

        if not Sm.is_DFA_compliant(): Sm = beautifier.do(Sm)
        self.sm_newline.set(Sm, sr)

    @typed(sr=SourceRef)
    def specify_suppressor(self, Sm, sr):
        _error_if_defined_before(self.sm_newline_suppressor, sr)

        self.count_command_map.add(Sm.get_beginning_character_set(), 
                                   "begin(newline suppressor)", None, sr)
        if not Sm.is_DFA_compliant(): Sm = beautifier.do(Sm)
        self.sm_newline_suppressor.set(Sm, sr)

    @typed(sr=SourceRef)
    def specify_comment(self, Sm, sr):
        _error_if_defined_before(self.sm_comment, sr)

        self.count_command_map.add(Sm.get_beginning_character_set(), 
                                   "begin(comment to newline)", None, sr)
        if not Sm.is_DFA_compliant(): Sm = beautifier.do(Sm)
        self.sm_comment.set(Sm, sr)

    def __sm_newline_default(self):
        """Default newline: '(\n)|(\r\n)'
        """
        global cc_type_name_db

        newline_set = NumberSet(ord('\n'))
        retour_set  = NumberSet(ord('\r'))

        before = self.count_command_map.find_occupier(newline_set, set())
        if before is not None:
            error.warning("Trying to implement default newline: '\\n' or '\\r\\n'.\n" 
                          "The '\\n' option is not possible, since it has been occupied by '%s'.\n" \
                          "No newline can be defined by default."
                          % cc_type_name_db[before.cc_type], before.sr, 
                          SuppressCode=NotificationDB.warning_default_newline_0A_impossible)
            # In this case, no newline can be defined!
            return

        sm = StateMachine.from_character_set(newline_set)

        if Setup.dos_carriage_return_newline_f:
            before = self.count_command_map.find_occupier(retour_set, set())
            if before is not None:
                error.warning("Trying to implement default newline: '\\n' or '\\r\\n'.\n" 
                          "The '\\r\\n' option is not possible, since '\\r' has been occupied by '%s'." \
                          % cc_type_name_db[before.cc_type],
                          before.sr, 
                          SuppressCode=NotificationDB.warning_default_newline_0D_impossible)
            else:
                sm.add_transition_sequence(sm.init_state_index, [retour_set, newline_set])

        return sm

    def __whitespace_default(self):
        """Try to define default whitespace ' ' or '\t' if their positions
        are not yet occupied in the count_command_map.
        """
        cs0 = NumberSet(ord(" "))
        cs1 = NumberSet(ord("\t"))
        result = NumberSet()
        if not self.count_command_map.find_occupier(cs0, set()):
            result.unite_with(cs0)
        if not self.count_command_map.find_occupier(cs1, set()):
            result.unite_with(cs1)

        if result.is_empty():
            error.log("Trying to implement default whitespace ' ' or '\\t' failed.\n"
                      "Characters are occupied by other elements.", self.sr)
        return result

    def finalize(self):
        if self.whitespace_character_set.get() is None:
            whitespace = self.__whitespace_default()
            self.__specify_character_set(self.whitespace_character_set, 
                                         "whitespace", whitespace, 
                                         sr=SourceRef_DEFAULT)
        if self.sm_newline.get() is None:
            sm_newline = self.__sm_newline_default()
            if sm_newline is not None: 
                self.specify_newline(sm_newline, SourceRef_DEFAULT)

        self.consistency_check()

    def consistency_check(self):
        self.count_command_map.check_defined(self.sr, E_CharacterCountType.WHITESPACE)
        self.count_command_map.check_defined(self.sr, E_CharacterCountType.BEGIN_NEWLINE)
        if self.sm_newline_suppressor.get() is not None:
            if self.sm_newline.get() is None:
                error.log("A newline 'suppressor' has been defined.\n"
                          "But there is no 'newline' in indentation defintion.", 
                          self.sm_newline_suppressor.sr)

    def __str__(self):
        def cs_str(Name, Cs):
            msg  = "%s:\n" % Name
            if Cs is None: msg += "    <none>\n" 
            else:          msg += "    %s\n" % Cs.get_utf8_string()
            return msg

        def sm_str(Name, Sm):
            msg = "%s:\n" % Name
            if Sm is None: 
                msg += "    <none>\n"
            else:          
                msg += "    %s\n" % Sm.get_string(NormalizeF=True, Option="utf8").replace("\n", "\n    ").strip()
            return msg

        return "".join([
            cs_str("Whitespace", self.whitespace_character_set.get()),
            cs_str("Bad",        self.bad_character_set.get()),
            sm_str("Newline",    self.sm_newline.get()),
            sm_str("Suppressor", self.sm_newline_suppressor.get()),
            sm_str("Comment",    self.sm_comment.get()),
        ])

def _error_set_intersection(CcType, Before, sr):
    global cc_type_name_db

    note_f = False
    if    CcType         == E_CharacterCountType.END_NEWLINE \
       or Before.cc_type == E_CharacterCountType.END_NEWLINE:
        note_f = True

    prefix = {
        E_CharacterCountType.COLUMN:                   "",
        E_CharacterCountType.GRID:                     "",
        E_CharacterCountType.LINE:                     "",
        E_CharacterCountType.BEGIN_NEWLINE_SUPPRESSOR: "beginning ",
        E_CharacterCountType.BEGIN_NEWLINE:            "beginning ",
        E_CharacterCountType.END_NEWLINE:              "ending ",
        E_CharacterCountType.BAD:                      "",
        E_CharacterCountType.WHITESPACE:               "",
        E_CharacterCountType.BEGIN_COMMENT_TO_NEWLINE: "beginning ",
    }[CcType]

    error.log("The %scharacter set defined in '%s' intersects" % (prefix, cc_type_name_db[CcType]),
              sr, DontExitF=True)
    error.log("with '%s' at this place." % cc_type_name_db[Before.cc_type], 
              Before.sr, DontExitF=note_f)

    if note_f:
        error.log("Note, for example, 'newline' cannot end with a character which is subject\n"
                  "to indentation counting (i.e. 'space' or 'grid').", sr)

def _error_if_defined_before(Before, sr):
    if not Before.set_f(): return

    error.log("'%s' has been defined before;" % Before.name, sr, 
              DontExitF=True)
    error.log("at this place.", Before.sr)

def extract_trigger_set(sr, Keyword, Pattern):
    if Pattern is None:
        return None
    elif isinstance(Pattern, NumberSet):
        return Pattern

    def check_can_be_matched_by_single_character(SM):
        bad_f      = False
        init_state = SM.get_init_state()
        if SM.get_init_state().is_acceptance(): 
            bad_f = True
        elif len(SM.states) != 2:
            bad_f = True
        # Init state MUST transit to second state. Second state MUST not have any transitions
        elif len(init_state.target_map.get_target_state_index_list()) != 1:
            bad_f = True
        else:
            tmp = set(SM.states.keys())
            tmp.remove(SM.init_state_index)
            other_state_index = tmp.__iter__().next()
            if len(SM.states[other_state_index].target_map.get_target_state_index_list()) != 0:
                bad_f = True

        if bad_f:
            error.log("For '%s' only patterns are addmissible which\n" % Keyword + \
                      "can be matched by a single character, e.g. \" \" or [a-z].", sr)

    check_can_be_matched_by_single_character(Pattern.sm)

    transition_map = Pattern.sm.get_init_state().target_map.get_map()
    assert len(transition_map) == 1
    return transition_map.values()[0]

_CounterSetupLineColumn_Default = None
def CounterSetupLineColumn_Default():
    global _CounterSetupLineColumn_Default

    if _CounterSetupLineColumn_Default is None:
        count_command_map = CountOpMap()
        count_command_map.add(NumberSet(ord('\n')), "newline", 1, SourceRef_DEFAULT)
        count_command_map.add(NumberSet(ord('\t')), "grid",    4, SourceRef_DEFAULT)
        count_command_map.define_else("space",   1, SourceRef_DEFAULT)    # Define: "\else"
        count_command_map.assign_else_count_command(
            Setup.buffer_codec.source_set.minimum(), 
            Setup.buffer_codec.source_set.supremum(),                     # Apply:  "\else"
            SourceRef_DEFAULT) 

        _CounterSetupLineColumn_Default = ParserDataLineColumn(SourceRef_DEFAULT, 
                                                               count_command_map)

    return _CounterSetupLineColumn_Default

