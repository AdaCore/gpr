# (C) Frank-Rene Schaefer
#______________________________________________________________________________
from   quex.engine.analyzer.door_id_address_label         import get_plain_strings, \
                                                                 dial_db
from   quex.engine.analyzer.terminal.core                 import Terminal
from   quex.output.core.variable_db        import variable_db
import quex.output.core.base                         as     generator
from   quex.engine.state_machine.engine_state_machine_set import EngineStateMachineSet
from   quex.engine.counter                                import CountOpFactory
from   quex.engine.misc.tools                                  import all_isinstance, \
                                                                 typed
import quex.output.cpp.counter                            as     counter
from   quex.input.regular_expression.construct            import Pattern
from   quex.blackboard                                    import setup as Setup, \
                                                                 E_IncidenceIDs, \
                                                                 Lng

@typed(ModeNameList = [(str, unicode)])
def do(Mode, ModeNameList):
    """RETURNS: The analyzer code for a mode defined in 'Mode'.
    """
    variable_db.init() 

    function_body,       \
    variable_definitions = do_core(Mode.pattern_list, 
                                   Mode.terminal_db,
                                   Mode.on_after_match_code)

    function_txt = wrap_up(Mode.name, function_body, variable_definitions, 
                           ModeNameList)

    return function_txt

@typed(PatternList=[Pattern], TerminalDb={(E_IncidenceIDs, long): Terminal})
def do_core(PatternList, TerminalDb, OnAfterMatchCode=None):
    """Produces main code for an analyzer function which can detect patterns given in
    the 'PatternList' and has things to be done mentioned in 'TerminalDb'. 

    RETURN: Code implementing the lexical analyzer.

    The code is not embedded in a function and required definitions are not provided.
    This happens through function 'wrap_up()'.
    """
    # Prepare the combined state machines and terminals 
    esms = EngineStateMachineSet(PatternList)

    # (*) Pre Context State Machine
    #     (If present: All pre-context combined in single backward analyzer.)
    pre_context, \
    pre_analyzer         = generator.do_pre_context(esms.pre_context_sm,
                                                    esms.pre_context_sm_id_list)
    # assert all_isinstance(pre_context, (IfDoorIdReferencedCode, int, str, unicode))

    # (*) Backward input position detection
    #     (Seldomly present -- only for Pseudo-Ambiguous Post Contexts)
    bipd                 = generator.do_backward_input_position_detectors(esms.bipd_sm_db)
    # assert all_isinstance(bipd, (IfDoorIdReferencedCode, int, str, unicode))

    # (*) Main State Machine -- try to match core patterns
    #     Post-context handling is webbed into the main state machine.
    main, \
    main_analyzer        = generator.do_main(esms.sm)
    # assert all_isinstance(main, (IfDoorIdReferencedCode, int, str, unicode))

    # (*) Terminals
    #     (BEFORE 'Reload procedures' because some terminals may add entries
    #      to the reloader.)
    terminals            = generator.do_terminals(TerminalDb.values(), 
                                                  main_analyzer)

    # (*) Reload procedures
    reload_procedure_fw  = generator.do_reload_procedure(main_analyzer)
    reload_procedure_bw  = generator.do_reload_procedure(pre_analyzer)

    # assert all_isinstance(reload_procedures, (IfDoorIdReferencedCode, int, str, unicode))

    # (*) Re-entry preparation
    reentry_preparation  = generator.do_reentry_preparation(esms.pre_context_sm_id_list,
                                                             OnAfterMatchCode)

    # (*) State Router
    #     (Something that can goto a state address by an given integer value)
    state_router         = generator.do_state_router()
    # assert all_isinstance(state_router, (IfDoorIdReferencedCode, int, str, unicode))

    # (*) Variable Definitions
    #     (Code that defines all required variables for the analyzer)
    variable_definitions = generator.do_variable_definitions()
    # assert all_isinstance(variable_definitions, (IfDoorIdReferencedCode, int, str, unicode))

    # (*) Putting it all together
    function_body = []
    function_body.extend(pre_context)         # implementation of pre-contexts (if there are some)
    function_body.extend(main)                # main pattern matcher
    function_body.extend(bipd)                # (seldom != empty; only for pseudo-ambiguous post contexts)
    function_body.extend(terminals)           
    function_body.extend(state_router)        # route to state by index (only if no computed gotos)
    function_body.extend(reload_procedure_fw)
    function_body.extend(reload_procedure_bw)
    function_body.extend(reentry_preparation)   

    return function_body, variable_definitions

def wrap_up(ModeName, FunctionBody, VariableDefs, ModeNameList):
    txt_function = Lng.ANALYZER_FUNCTION(ModeName, Setup, VariableDefs, 
                                         FunctionBody, ModeNameList) 
    txt_header   = Lng.HEADER_DEFINITIONS() 
    assert isinstance(txt_header, (str, unicode))

    txt_analyzer = get_plain_strings(txt_function)
    assert all_isinstance(txt_analyzer, (str, unicode))

    return [ txt_header ] + txt_analyzer

def do_default_counter(Mode):
    if not Mode.default_character_counter_required_f:
        return []

    dial_db.clear()
    ccfactory = CountOpFactory.from_ParserDataLineColumn(
                    Mode.counter_db, 
                    Setup.buffer_codec.source_set, 
                    Lng.INPUT_P())

    variable_db.init()

    # May be, the default counter is the same as for another mode. In that
    # case call the default counter of the other mode with the same one and
    # only macro.
    default_character_counter_function_name,   \
    default_character_counter_function_code  = counter.get(ccfactory, Mode.name)

    txt = [ Lng.DEFAULT_COUNTER_PROLOG(default_character_counter_function_name) ]

    if default_character_counter_function_code is not None:
        txt.append(default_character_counter_function_code)

    return txt

def frame_this(Code):
    return Lng["$frame"](Code, Setup)


