from   quex.input.regular_expression.construct         import Pattern
import quex.engine.state_machine.algorithm.beautifier  as     beautifier
from   quex.engine.state_machine.core                  import StateMachine
import quex.engine.state_machine.construction.parallelize           as     parallelize
import quex.engine.state_machine.transformation.core   as     transformation
from   quex.engine.analyzer.door_id_address_label      import dial_db
from   quex.engine.operations.operation_list                       import Op
import quex.engine.misc.error                          as     error
from   quex.engine.misc.tools                          import all_isinstance, \
                                                              all_true, \
                                                              concatinate, \
                                                              typed

from   quex.blackboard import setup as Setup, \
                              E_R

class EngineStateMachineSet:
    def __init__(self, PatternList): 
        assert isinstance(PatternList, list)
        assert len(PatternList) > 0
        assert all_isinstance(PatternList, Pattern)
        assert all_true(PatternList, lambda p: p.incidence_id() is not None)

        # (*) Core SM, Pre-Context SM, ...
        #     ... and sometimes backward input position SMs.
        self.sm,                    \
        self.pre_context_sm,        \
        self.bipd_sm_db,            \
        self.pre_context_sm_id_list = self.__prepare(PatternList)

    def __prepare(self, PatternList):
        # -- setup of state machine lists and id lists
        core_sm_list,                 \
        pre_context_sm_list,          \
        incidence_id_and_bipd_sm_list = self.__prepare_sm_lists(PatternList)

        # (*) Create (combined) state machines
        #     Backward input position detection (bipd) remains separate engines.
        return get_combined_state_machine(core_sm_list),                  \
               get_combined_state_machine(pre_context_sm_list,            \
                                          FilterDominatedOriginsF=False), \
               dict((incidence_id, sm) for incidence_id, sm in incidence_id_and_bipd_sm_list), \
               [ sm.get_id() for sm in pre_context_sm_list ]

    def __prepare_sm_lists(self, PatternList):
        # -- Core state machines of patterns
        state_machine_list = [ pattern.sm for pattern in PatternList ]

        # -- Pre-Contexts
        pre_context_sm_list = [    
            pattern.pre_context_sm for pattern in PatternList \
            if pattern.pre_context_sm is not None 
        ]

        # -- Backward input position detection (BIPD)
        bipd_sm_list = [
            (pattern.incidence_id(), pattern.bipd_sm) for pattern in PatternList \
            if pattern.bipd_sm is not None 
        ]
        return state_machine_list, pre_context_sm_list, bipd_sm_list

class CharacterSetStateMachine:
    @typed(IncidenceIdMap=list, MaintainLexemeF=bool)
    def __init__(self, IncidenceIdMap, MaintainLexemeF, ParallelSmList=None, 
                 OnBegin=None, OnEnd=None, OnBeforeReload=None, OnAfterReload=None, 
                 BeyondIid=None):
        """Brief: Generates a state machine that implements the transition
        to terminals upon the input falling into a number set. 
            
                   .-----------------.
                   | character set 0 +---- - -> Incidence0
                   |                 |
                   | character set 1 +---- - -> Incidence1
                   |                 |
                   | character set 2 +---- - -> Incidence2
                   '-----------------'         

        The terminals related to the mentioned incidence ids are not implemented.
        If Setup.buffer_codec is defined the state machine
        is transformed accordingly.

        MaintainLexemeF == True => The lexeme_start_p is maintained. This restricts
                                   the amount of content which can be loaded into 
                                   the buffer.

        ARGUMENTS:

        IncidenceIdMap: List of tuples (NumberSet, IncidenceId) 

        ParallelSmList: List of state machines which are supposed to be mounted
                        in parallel to the root incidence map.

        """
        def trafo(SM):
            dummy, sm = transformation.do_state_machine(SM)
            assert sm is not None
            return sm

        self.maintain_lexeme_f = MaintainLexemeF

        # Perform a character set transformation, if required.
        # State machine ids of parallel state machines must be lower than
        # main state machine. Thus, transform first.
        psm_list = []
        if ParallelSmList is not None:
            psm_list = [trafo(sm) for sm in ParallelSmList]
        main_sm  = trafo(self.__prepare_incidence_id_map(IncidenceIdMap))

        self.sm = get_combined_state_machine(psm_list + [main_sm],
                                             MarkNotSet=set([main_sm.get_id()]))

        self.__prepare_begin_and_putback(OnBegin, OnEnd)
        self.__prepare_before_and_after_reload(OnBeforeReload, OnAfterReload)

        self.incidence_id_beyond = BeyondIid

    @staticmethod
    def from_CountOpFactory(ccfactory, LexemeMaintainedF, ParallelSmList=None):
        """The function 'get_terminal_list' returns a list of termins which 
        fit the incidence ids of the counting actions.
        """
        beyond_iid = dial_db.new_incidence_id()
        # Build a state machine based on (character set, incidence_id) pairs.
        return CharacterSetStateMachine(
                   ccfactory.get_incidence_id_map(beyond_iid), 
                   LexemeMaintainedF, 
                   ParallelSmList, 
                   OnBegin        = ccfactory.on_begin, 
                   OnEnd          = ccfactory.on_end,
                   OnBeforeReload = ccfactory.on_before_reload,
                   OnAfterReload  = ccfactory.on_after_reload, 
                   BeyondIid      = beyond_iid)

    def __prepare_begin_and_putback(self, OnBegin, OnEnd):
        """If we deal with variable character sizes, the begin of the letter is stored
        in 'character_begin_p'. To reset the input pointer 'input_p = character_begin_p' 
        is applied.
        """
        if not Setup.buffer_codec.variable_character_sizes_f():
            # 1 character == 1 chunk
            # => rest to last character: 'input_p = input_p - 1'
            self.on_step    = []
            self.on_putback = [ Op.InputPDecrement() ]
        else:
            # 1 character == variable number of chunks
            # => store begin of character in 'lexeme_start_p'
            # => rest to last character: 'input_p = lexeme_start_p'
            self.on_step    = [ Op.Assign(E_R.CharacterBeginP, E_R.InputP) ]
            self.on_putback = [ Op.Assign(E_R.InputP, E_R.CharacterBeginP) ]
        self.on_begin = concatinate(self.on_step, OnBegin)
        self.on_end   = concatinate(self.on_putback, OnEnd)

    def __prepare_before_and_after_reload(self, OnBeforeReload, OnAfterReload):
        """The 'lexeme_start_p' restricts the amount of data which is loaded 
        into the buffer upon reload--if the lexeme needs to be maintained. If 
        the lexeme does not need to be maintained, then the whole buffer can 
        be refilled.
        
        For this, the 'lexeme_start_p' is set to the input pointer. 
        
        EXCEPTION: Variable character sizes. There, the 'lexeme_start_p' is used
        to mark the begin of the current letter. However, letters are short, so 
        the drawback is tiny.

        RETURNS: [0] on_before_reload
                 [1] on_after_reload
        """
        if Setup.buffer_codec.variable_character_sizes_f():
            if not self.maintain_lexeme_f:
                on_before_reload = [ Op.Assign(E_R.LexemeStartP, E_R.CharacterBeginP) ]
                on_after_reload  = [ Op.Assign(E_R.CharacterBeginP, E_R.LexemeStartP) ]
            else:
                assert False
                # Here, the character begin p needs to be adapted to what has been reloaded.
                on_before_reload = [ ] # LexemeBegin is enough.
                on_after_reload  = [ ]
        else:
            on_before_reload = [ Op.Assign(E_R.LexemeStartP, E_R.InputP) ] 
            on_after_reload  = [ ] # Op.Assign(E_R.InputP, E_R.LexemeStartP) ]

        self.on_before_reload = concatinate(on_before_reload,OnBeforeReload)
        self.on_after_reload  = concatinate(on_after_reload, OnAfterReload)

    def __prepare_incidence_id_map(self, IncidenceIdMap):
        def add(sm, StateIndex, TriggerSet, IncidenceId):
            if TriggerSet.is_empty(): return
            target_state_index = sm.add_transition(StateIndex, TriggerSet)
            target_state       = sm.states[target_state_index]
            target_state.set_acceptance()
            target_state.mark_acceptance_id(IncidenceId)

        sm = StateMachine()
        for character_set, incidence_id in IncidenceIdMap:
            # 'cliid' = unique command list incidence id.
            add(sm, sm.init_state_index, character_set, incidence_id)

        return sm

def get_combined_state_machine(StateMachine_List, FilterDominatedOriginsF=True,
                               MarkNotSet=set()):
    """Creates a DFA state machine that incorporates the paralell
       process of all pattern passed as state machines in 
       the StateMachine_List. Each origins of each state machine
       are kept in the final state, if it is not dominated.

       Performs: -- parallelization
                 -- translation from NFA to DFA
                 -- Frank Schaefers Adapted Hopcroft optimization.

       Again: The state machine ids of the original state machines
              are traced through the whole process.
              
       FilterDominatedOriginsF, if set to False, can disable the filtering
              of dominated origins. This is important for pre-contexts, because,
              all successful patterns need to be reported!            
                      
    """   
    if len(StateMachine_List) == 0:
        return None

    def __check(Place, sm):
        __check_on_orphan_states(Place, sm)
        __check_on_init_state_not_acceptance(Place, sm)

    def __check_on_orphan_states(Place, sm):
        orphan_state_list = sm.get_orphaned_state_index_list()
        if len(orphan_state_list) == 0: return
        error.log("After '%s'" % Place + "\n" + \
                  "Orphaned state(s) detected in regular expression (optimization lack).\n" + \
                  "Please, log a defect at the projects website quex.sourceforge.net.\n"    + \
                  "Orphan state(s) = " + repr(orphan_state_list)) 

    def __check_on_init_state_not_acceptance(Place, sm):
        if sm.get_init_state().is_acceptance():
            error.log("After '%s'" % Place + "\n" + \
                      "Initial state 'accepts'. This should never happen.\n" + \
                      "Please, log a defect at the projects web site quex.sourceforge.net.\n")

    # (1) mark at each state machine the machine and states as 'original'.
    #      
    #     This is necessary to trace in the combined state machine the
    #     pattern that actually matched. Note, that a state machine in
    #     the StateMachine_List represents one possible pattern that can
    #     match the current input.   
    #
    for sm in StateMachine_List:
        if sm.get_id() in MarkNotSet: continue
        sm.mark_state_origins()
        assert sm.is_DFA_compliant(), sm.get_string(Option="hex")

    # (2) setup all patterns in paralell 
    sm = parallelize.do(StateMachine_List, CommonTerminalStateF=False) #, CloneF=False)
    __check("Parallelization", sm)

    # (4) determine for each state in the DFA what is the dominating original state
    if FilterDominatedOriginsF: sm.filter_dominated_origins()
    __check("Filter Dominated Origins", sm)

    # (3) convert the state machine to an DFA (paralellization created an NFA)
    sm = beautifier.do(sm)
    __check("NFA to DFA, Hopcroft Minimization", sm)
    
    return sm

