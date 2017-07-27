# (C) 2005-2011 Frank-Rene Schaefer
# ABSOLUTELY NO WARRANTY
###############################################################################
from   quex.engine.state_machine.core          import StateMachine
from   quex.engine.state_machine.state.core    import State
import quex.engine.state_machine.index         as index
import quex.engine.state_machine.check.special as special
from   quex.engine.misc.tools import typed

@typed(StateMachineList=[StateMachine])
def do(StateMachineList, CommonTerminalStateF=True, CloneF=True):
    """Connect state machines paralell.

       CommonTerminalStateF tells whether the state machines shall trigger 
                            to a common terminal. This may help nfa-to-dfa
                            or hopcroft minimization for ISOLATED patterns.

                            A state machine that consists of the COMBINATION
                            of patterns MUST set this flag to 'False'.

       CloneF               Controls if state machine list is cloned or not.
                            If the single state machines are no longer required after
                            construction, the CloneF can be set to False.

                            If Cloning is disabled the state machines themselves
                            will be altered--which brings some advantage in speed.
    """
    assert len(StateMachineList) != 0
              
    # filter out empty state machines from the consideration          
    state_machine_list       = [ sm for sm in StateMachineList if not (sm.is_empty() or special.is_none(sm))]
    empty_state_machine_list = [ sm for sm in StateMachineList if     (sm.is_empty() or special.is_none(sm))]

    if len(state_machine_list) < 2:
        if len(state_machine_list) < 1: result = StateMachine()
        elif CloneF:                    result = state_machine_list[0].clone()
        else:                           result = state_machine_list[0]

        return __consider_empty_state_machines(result, empty_state_machine_list)

    # (*) need to clone the state machines, i.e. provide their internal
    #     states with new ids, but the 'behavior' remains. This allows
    #     state machines to appear twice, or being used in 'larger'
    #     conglomerates.
    if CloneF: clone_list = map(lambda sm: sm.clone(), state_machine_list)
    else:      clone_list = state_machine_list

    # (*) collect all transitions from both state machines into a single one
    #     (clone to ensure unique identifiers of states)
    new_init_state = State() 
    result         = StateMachine(InitState=new_init_state)

    for clone in clone_list:
        result.states.update(clone.states)

    # (*) add additional **init** and **end** state
    #     NOTE: when the result state machine was created, it already contains a 
    #           new initial state index. thus at this point only the new terminal
    #           state has to be created. 
    #     NOTE: it is essential that the acceptance flag stays False, at this
    #           point in time, so that the mounting operations only happen on
    #           the old acceptance states. Later the acceptance state is raised
    #           to 'accepted' (see below)
    new_terminal_state_index = -1L
    if CommonTerminalStateF:
        new_terminal_state_index = index.get()
        result.states[new_terminal_state_index] = State() 
    
    # (*) Connect from the new initial state to the initial states of the
    #     clones via epsilon transition. 
    #     Connect from each success state of the clones to the new terminal
    #     state via epsilon transition.
    for clone in clone_list:
        result.mount_to_initial_state(clone.init_state_index)

    if CommonTerminalStateF:
        result.mount_to_acceptance_states(new_terminal_state_index,
                                          CancelStartAcceptanceStateF=False)

    return __consider_empty_state_machines(result, empty_state_machine_list)

def __consider_empty_state_machines(sm, EmptyStateMachineList):
    """An empty state machine basically means that its init state is going to 
       be merge into the init state of the resulting state machine. 

       If there is an empty state machine with an acceptance, then this is 
       reflected in the origins. Thus, the result's init state becomes an
       acceptance state for that pattern. 
       
       => There is no particular need for an epsilon transition to the common
          new terminal index.
    """
    single_entry = sm.get_init_state().single_entry
    for esm in EmptyStateMachineList:
        single_entry.merge(esm.get_init_state().single_entry)
    return sm

