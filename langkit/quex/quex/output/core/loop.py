from   quex.engine.state_machine.engine_state_machine_set import CharacterSetStateMachine
import quex.engine.analyzer.core                  as     analyzer_generator
from   quex.engine.operations.operation_list                  import Op, \
                                                         OpList
import quex.engine.state_machine.index            as     index
from   quex.engine.analyzer.door_id_address_label import DoorID
from   quex.engine.misc.tools                     import typed
from   quex.output.core.variable_db               import variable_db
import quex.output.core.base                      as     generator
from   quex.blackboard import E_StateIndices, \
                              E_R, \
                              E_CharacterCountType, \
                              setup as Setup

@typed(ReloadF=bool, LexemeEndCheckF=bool, AfterBeyond=list)
def do(CcFactory, AfterBeyond, LexemeEndCheckF=False, EngineType=None, ReloadStateExtern=None, LexemeMaintainedF=False,
       ParallelSmTerminalPairList=None):
    """Generates a (pseudo-one-state) state machine with the properties:
        
               Buffer Limit Code --> Reload
               Loop Character    --> Loop Entry
               Else              --> Exit Loop

    NOTE: This function does NOT code the FAILURE terminal. The caller needs to 
          do this if required.

    Generate code to iterate over the input stream until

           -- A character occurs not in CharacterSet, or
           -- [optional] the 'LexemeEnd' is reached.

    That is, simplified:
                             input in Set
                             .--<--.
                            |      |  LexemeEnd
                            |      +----->------> (Exit)
                          .----.   |
               --------->( Loop )--+----->------> Exit
                          '----'       input 
                                     not in Set
        
    At the end of the iteration, the 'input_p' points to (the begin of) the
    first character which is not in CharacterSet (or the LexemeEnd).

            [i][i][i]..................[i][i][X][.... 
                                             |
                                          input_p
            
    During the 'loop' possible line/column count commands may be applied. To
    achieve the iteration, a simplified pattern matching engine is implemented:

              transition
              map
              .------.  
              |  i0  |----------> Terminal0: OpList0   
              +------+
              |  i1  |----------> Terminal1: OpList1   
              +------+
              |  X2  |----------> Terminal Beyond: input_p--; goto TerminalExit;
              +------+
              |  i2  |----------> Terminal2: OpList2
              +------+
    """
    assert EngineType is not None
    # NOT: assert (not EngineType.subject_to_reload()) or ReloadStateExtern is None
    # This would mean, that the user has to make these kinds of decisions. But, 
    # we are easily able to ignore meaningless ReloadStateExtern objects.

    # (*) Construct State Machine and Terminals _______________________________
    #
    parallel_sm_list = None
    if ParallelSmTerminalPairList is not None:
        parallel_sm_list = [ sm for sm, terminal in ParallelSmTerminalPairList ]

    CsSm = CharacterSetStateMachine.from_CountOpFactory(CcFactory, 
                                                         LexemeMaintainedF,
                                                         ParallelSmList=parallel_sm_list)

    analyzer = analyzer_generator.do(CsSm.sm, EngineType,
                                     ReloadStateExtern,
                                     OnBeforeReload = OpList.from_iterable(CsSm.on_before_reload), 
                                     OnAfterReload  = OpList.from_iterable(CsSm.on_after_reload))

    # -- The terminals 
    #
    door_id_loop = _prepare_entry_and_reentry(analyzer, CsSm.on_begin, CsSm.on_step) 

    def get_LexemeEndCheck_appendix(ccfactory, CC_Type):
        if not LexemeEndCheckF: 
            return [ Op.GotoDoorId(door_id_loop) ]
        #
        #       .---------------.        ,----------.   no
        #   --->| Count Op |-------< LexemeEnd? >------> DoorIdOk
        #       '---------------'        '----+-----'
        #                                     | yes
        #                              .---------------.
        #                              |  Lexeme End   |
        #                              | Count Op |----> DoorIdOnLexemeEnd
        #                              '---------------'
        #  
        elif ccfactory.requires_reference_p() and CC_Type == E_CharacterCountType.COLUMN: 
            return [
                Op.GotoDoorIdIfInputPNotEqualPointer(door_id_loop, E_R.LexemeEnd),
                Op.ColumnCountReferencePDeltaAdd(E_R.InputP, ccfactory.column_count_per_chunk, False),
            ] + AfterBeyond
        else:
            return [
                Op.GotoDoorIdIfInputPNotEqualPointer(door_id_loop, E_R.LexemeEnd),
            ] + AfterBeyond

    terminal_list = CcFactory.get_terminal_list(CsSm.on_end + AfterBeyond,
                                                CsSm.incidence_id_beyond,
                                                get_LexemeEndCheck_appendix)
    if ParallelSmTerminalPairList is not None:
        terminal_list.extend(
            terminal for sm, terminal in ParallelSmTerminalPairList
        )

    # (*) Generate Code _______________________________________________________
    txt = _get_source_code(CcFactory, analyzer, terminal_list)
    
    return txt, DoorID.incidence(CsSm.incidence_id_beyond)

def _prepare_entry_and_reentry(analyzer, OnBegin, OnStep):
    """Prepare the entry and re-entry doors into the initial state
    of the loop-implementing initial state.

                   .----------.
                   | on_entry |
                   '----------'
                        |         .------------.
                        |<--------| on_reentry |<-----.
                        |         '------------'      |
                .----------------.                    |
                |                +-----> Terminal ----+----> Exit
                |      ...       |
                |                +-----> Terminal - - 
                '----------------'

       RETURNS: DoorID of the re-entry door which is used to iterate in 
                the loop.
    """
    # Entry into state machine
    entry            = analyzer.init_state().entry
    init_state_index = analyzer.init_state_index
        
    # OnEntry
    ta_on_entry              = entry.get_action(init_state_index, 
                                                E_StateIndices.BEFORE_ENTRY)
    ta_on_entry.command_list = OpList.concatinate(ta_on_entry.command_list, 
                                                       OnBegin)

    # OnReEntry
    tid_reentry = entry.enter_OpList(init_state_index, index.get(), 
                                          OpList.from_iterable(OnStep))
    entry.categorize(init_state_index)

    return entry.get(tid_reentry).door_id

def _get_source_code(CcFactory, analyzer, terminal_list):
    """RETURNS: String containing source code for the 'loop'. 

       -- The source code for the (looping) state machine.
       -- The terminals which contain counting actions.

    Also, it requests variable definitions as they are required.
    """
    txt = []
    txt.extend(
        generator.do_analyzer(analyzer)
    )
    txt.extend(
        generator.do_terminals(terminal_list, analyzer)
    )
    if analyzer.engine_type.subject_to_reload():
        txt.extend(
            generator.do_reload_procedure(analyzer)
        )

    if CcFactory.requires_reference_p():   
        variable_db.require("reference_p", Condition="QUEX_OPTION_COLUMN_NUMBER_COUNTING")
    if Setup.buffer_codec.variable_character_sizes_f(): 
        variable_db.require("character_begin_p")
    return txt
