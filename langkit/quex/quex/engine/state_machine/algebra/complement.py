"""Algebraic relations:

    inverse(\Any*} == \None
    \Not{\None}    == \Any*

    inverse(intersection(A, B)) == union(inverse(A), inverse(B))
    inverse(union(A, B))        == intersection(inverse(A), inverse(B))

    (C) 2013 Frank-Rene Schaefer
"""
import quex.engine.state_machine.index      as     index
from   quex.engine.state_machine.state.core import State
from   quex.engine.misc.interval_handling        import NumberSet, Interval
from   quex.blackboard                      import setup as Setup
from   copy import deepcopy
import sys

def do(SM):
    """RETURNS: A state machines that matches anything which is 
               not matched by SM.

       Idea: The paths along SM do not guide to acceptance states,
             but to normal states.

             Any drop-out is translated into a transition into 
             the 'accept all state'.

       NOTE: This function produces a finite state automaton which
             is not applicable by itself. It would eat ANYTHING
             from a certain state on.
    """
    result = deepcopy(SM) # Not clone

    accept_all_state_index = index.get()
    state = State(AcceptanceF=True)
    state.add_transition(NumberSet(Interval(-sys.maxint, sys.maxint)), 
                         accept_all_state_index)
    result.states[accept_all_state_index] = state

    def is_accept_all_state(sm, StateIndex):
        state = sm.states[StateIndex]
        if not state.is_acceptance():                return False
        tm    = state.target_map.get_map()
        if len(tm) != 1:                             return False
        elif tm.iterkeys().next() != StateIndex:     return False
        elif not tm.itervalues().next().is_all():    return False

        # Target is an 'Accept-All' state. Delete the transition.
        return True

    for state_index, state in SM.states.iteritems():
        # deepcopy --> use same state indices in SM and result
        result_state = result.states[state_index]
        assert state.target_map.is_DFA_compliant(), \
               "State machine must be transformed to DFA first: nfa_to_dfa.do()"

        # -- Every transition to 'Accept-All' state becomes a drop-out.
        for target_index in (i for i in state.target_map.get_target_state_index_list()
                               if is_accept_all_state(SM, i)):
            result_state.target_map.delete_transitions_to_target(target_index)

        # -- Every drop-out becomes a transition to 'Accept-All' state.
        trigger_set         = state.target_map.get_trigger_set_union()
        inverse_trigger_set = trigger_set.get_complement(Setup.buffer_codec.source_set)
        if not inverse_trigger_set.is_empty():
            result_state.add_transition(inverse_trigger_set, accept_all_state_index)

    # Every acceptance state becomes a non-acceptance state.
    # Every non-acceptance state becomes an acceptance state.
    for state_index, state in SM.states.iteritems():
        if state.is_acceptance(): 
            result.states[state_index].set_acceptance(False)
        elif state_index != SM.init_state_index:
            result.states[state_index].set_acceptance(True)

    result.clean_up()

    return result.clone()

