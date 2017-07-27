"""____________________________________________________________________________
(C) 2013 Frank-Rene Schaefer
_______________________________________________________________________________
"""
import quex.engine.state_machine.character_counter as character_counter
from   quex.engine.misc.tools import typed
from   quex.blackboard import E_Count, \
                              E_IncidenceIDs, \
                              Lng

def get(ThePattern, ShiftF=True):
    """Line and column number actions for a pattern.

    Generates code to adapt line and column number counters when a pattern
    has matched. It tries to do as much as possible beforehand. That is, if
    the line and column increments are determined from the pattern structure
    it produces very simple and fast instructions.

    This function assumes that the 'count_info' for a pattern has been
    determined before, based on the 'counted_db'.
    
    If the adaption of line and column numbers cannot be derived from the 
    pattern itself or the lexeme length, then a call to the 'default_counter'
    is implemented. 

    ---------------------------------------------------------------------------

    RETURN: Verdict, CounterCode

        Verdict == True  --> Pattern requires run-time counting. Default
                             counter implementation required.
                   False --> It was possible to determine the increments
                             based on the pattern's structure. Non run-
                             time counting is necessary.

        CounterCode = Code to be prefixed in from of the action.
    ---------------------------------------------------------------------------
    
    The increment of line number and column number may be determined by
    'ThePattern' itself. For example the pattern "\n\n" increments the line
    number always by 2. The pattern "\n+" however increments the line number
    depending on how many '\n' it matches at runtime. These considerations
    where done by means of 

      quex.engine.state_machine.character_counter.CountInfo.from_StateMachine(...)

    It is called inside the 'prepare_count_info()' member function of the
    pattern at the time when it is communicated to the 'Mode' object from the
    'ModeDescription' object in:

              quex.input.files.mode.Mode.__init__(...)

    As a consequence of a call to '.prepare_count_info()', the pattern's 'count'
    object must be set to something not 'None'. If it is 'None', this means
    that the 'prepare_count_info()' function has not been called for it.  
    ---------------------------------------------------------------------------
    """
    
    assert type(ShiftF) == bool

    # (*) Trivial Cases _______________________________________________________
    if ThePattern.incidence_id() == E_IncidenceIDs.END_OF_STREAM:
        if ShiftF: return False, ["__QUEX_IF_COUNT_SHIFT_VALUES();\n" ]
        else:      return False, []

    if ThePattern is None:
        # 'on_failure' ... count any appearing character
        return True, [ Lng.DEFAULT_COUNTER_CALL() ]

    return do_CountInfo(ThePattern.count_info(), ShiftF)

@typed(counter=character_counter.CountInfo)
def do_CountInfo(counter, ShiftF=True):
    # (*) Default Character Counter ___________________________________________
    #
    #     Used when the increments and/or setting cannot be derived from the 
    #     pattern itself. That is, if one of the following is VOID:
    if    (    counter.line_n_increment_by_lexeme_length   == E_Count.VOID \
           and counter.line_n_increment                    == E_Count.VOID)\
       or (    counter.column_n_increment_by_lexeme_length == E_Count.VOID \
           and counter.column_n_increment                  == E_Count.VOID \
           and counter.column_index                        == E_Count.VOID \
           and counter.grid_step_size_by_lexeme_length     == E_Count.VOID):
        return True, [ Lng.DEFAULT_COUNTER_CALL() ]

    # (*) Determine Line and Column Number Count ______________________________
    #    
    #     Both, for line and column number considerations the same rules hold.
    #     Those rules are defined in 'get_increment()' as shown below.
    #
    def get_increment(txt, Increment, IncrementByLexemeLength, HelpStr):
        if IncrementByLexemeLength == 0 or Increment == 0:
            return 
        elif Increment != E_Count.VOID:
            arg = Lng.VALUE_STRING(Increment)
        else:
            arg = Lng.MULTIPLY_WITH("LexemeL", IncrementByLexemeLength)

        txt.append("__QUEX_IF_COUNT_%s_ADD(%s);\n" % (HelpStr, arg))

    # Column and line counts must be shifted (begin=end) even if only
    # columns are counted. For example, even if only columns are modified
    # the old line_number_at_begin must be adapted to the current.
    if ShiftF: txt = ["__QUEX_IF_COUNT_SHIFT_VALUES();\n" ]
    else:      txt = []

    # -- Line Number Count
    get_increment(txt, counter.line_n_increment, 
                  counter.line_n_increment_by_lexeme_length, 
                  "LINES")

    # -- Column Number Count
    if  counter.column_index != E_Count.VOID:
        txt.append("__QUEX_IF_COUNT_COLUMNS_SET(%i);\n" % (counter.column_index + 1))

    elif counter.column_n_increment_by_lexeme_length != E_Count.VOID:
        get_increment(txt, counter.column_n_increment, 
                      counter.column_n_increment_by_lexeme_length, 
                      "COLUMNS")

    else:
        # Following assert results from entry check against 'VOID'
        assert counter.grid_step_size_by_lexeme_length != E_Count.VOID

        if   counter.grid_step_n == E_Count.VOID: 
            grid_step_n = "LexemeL"
        elif counter.grid_step_n != 0:
            grid_step_n = counter.grid_step_n
        else:
            grid_step_n = None

        if grid_step_n is not None:
            txt.extend(Lng.GRID_STEP("self.counter._column_number_at_end", "size_t",
                                            counter.grid_step_size_by_lexeme_length, 
                                            grid_step_n, IfMacro="__QUEX_IF_COUNT_COLUMNS"))
            txt.append("\n")

    return False, txt
