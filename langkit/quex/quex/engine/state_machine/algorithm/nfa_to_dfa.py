from   quex.engine.state_machine.core        import StateMachine
from   quex.engine.state_machine.state.core  import State
from   quex.engine.state_machine.index       import map_state_combination_to_index

def do(SM, Class_StateMachine=StateMachine, Class_State=State):
    """Creates a deterministic finite automaton (DFA) from a state machine 
    - which may be a NFA (non-deterministic finite automaton). 
    
    This is a generalized version of the 'subset construction' algorithm. Where
    subsection construction focuses on letters of an alphabet for the
    investigation of transitions, this algorithm focuses on elementary trigger
    sets. A very good description of the subset construction algorithm can be
    found in 'Engineering a Compiler' by Keith Cooper.
    """
    # (*) create the result state machine
    initial_state_epsilon_closure = SM.get_epsilon_closure(SM.init_state_index) 

    # (*) initial state of resulting DFA = epsilon closure of initial state of NFA
    #     -- add the command list of all states in the epsilon closure
    InitState = Class_State.from_state_iterable(
                           SM.states[i] for i in initial_state_epsilon_closure)

    # NOTE: 
    # State machines with an initial acceptance state are conceivable!  In a
    # 'define' section building bricks of patterns may be defined that 'accept
    # nothing'. Those 'building bricks' may use nfa_to_dfa here, too.  
    #
    # (A pattern state machine for pattern matching, of course, has to disallow 
    #  'accept nothing'.)
    result = Class_StateMachine(InitState=InitState)
                          
    # (*) prepare the initial worklist
    worklist = [ ( result.init_state_index, initial_state_epsilon_closure) ]

    epsilon_closure_db = SM.get_epsilon_closure_db()

    while len(worklist) != 0:
        # 'start_state_index' is the index of an **existing** state in the state machine.
        # It was either created above, in StateMachine's constructor, or as a target
        # state index.
        start_state_index, start_state_combination = worklist.pop()
 
        # (*) compute the elementary trigger sets together with the 
        #     epsilon closure of target state combinations that they trigger to.
        #     In other words: find the ranges of characters where the state triggers to
        #     a unique state combination. E.g:
        #                Range        Target State Combination 
        #                [0:23]   --> [ State1, State2, State10 ]
        #                [24:60]  --> [ State1 ]
        #                [61:123] --> [ State2, State10 ]
        #
        elementary_trigger_set_infos = SM.get_elementary_trigger_sets(start_state_combination,
                                                                      epsilon_closure_db)
        ## DEBUG_print(start_state_combination, elementary_trigger_set_infos)

        # (*) loop over all elementary trigger sets
        for epsilon_closure_of_target_state_combination, trigger_set in elementary_trigger_set_infos.iteritems():
            #  -- if there is no trigger to the given target state combination, then drop it
            if trigger_set.is_empty(): continue

            # -- add a new target state representing the state combination
            #    (if this did not happen yet)
            target_state_index = \
                 map_state_combination_to_index(epsilon_closure_of_target_state_combination)

            # -- if target state combination was not considered yet, then create 
            #    a new state in the state machine
            if not result.states.has_key(target_state_index):
                # create the new target state in the state machine
                result.states[target_state_index] = \
                    Class_State.from_state_iterable(
                        SM.states[i] 
                        for i in epsilon_closure_of_target_state_combination)

                worklist.append((target_state_index, 
                                 epsilon_closure_of_target_state_combination))  

            # -- add the transition 'start state to target state'
            result.add_transition(start_state_index, trigger_set, target_state_index)

    return result 

def DEBUG_print(start_state_combination, elementary_trigger_list):
    print "----"
    print "StartStateCombination:", start_state_combination
    for ti, trigger_set in elementary_trigger_list:
        print "trigger set =", trigger_set.get_utf8_string(), "target =", ti 
    print "----"

