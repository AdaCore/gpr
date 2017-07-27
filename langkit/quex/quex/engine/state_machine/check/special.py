import quex.engine.state_machine.algorithm.beautifier as     beautifier
from   quex.engine.state_machine.core                 import StateMachine
from   quex.engine.state_machine.state.core           import State
import quex.engine.state_machine.index                as     index
from   quex.engine.misc.interval_handling                  import NumberSet, Interval

import sys

def get_all():
    """RETURNS:

       A state machine that 'eats' absolutely everything, i.e. 


                              .--- \Any ---.
                              |            |
           (0)--- \Any --->(( 0 ))<--------'
    """
    result = StateMachine()

    i      = index.get()
    state  = State(AcceptanceF=True)
    state.add_transition(NumberSet(Interval(-sys.maxint, sys.maxint)), i)
    result.states[i] = state

    result.get_init_state().add_transition(NumberSet(Interval(-sys.maxint, sys.maxint)), i)

    return result

def get_any():
    """RETURNS:

       A state machine that 'eats' any character, but only one. 

           (0)--- \Any --->(( 0 ))
    """
    result = StateMachine()
    result.add_transition(result.init_state_index, NumberSet(Interval(-sys.maxint, sys.maxint)), AcceptanceF=True)

    return result


def get_none():
    return StateMachine()

def is_none(SM):
    """Does the given state machine represent a pattern which 
    matches absolutely nothing?
    """
    sm = beautifier.do(SM)
    if   len(sm.states) != 1:                 return False
    elif sm.get_init_state().is_acceptance(): return False
    else:                                     return True

def is_all(SM):
    """Pattern has only two states: the init state which is not 
    accepting, and the accepting state which transits to itself
    forever.
    """
    sm = beautifier.do(SM)
    # Init State:
    #   -- not an acceptance state
    #   -- has only one transition on 'all' to an acceptance state
    #
    if   len(sm.states) != 2:                 return False
    init_state = sm.get_init_state()
    if   init_state.is_acceptance():          return False
    tm = init_state.target_map.get_map()
    if   len(tm) != 1:                        return False
    target_index, trigger_set = tm.iteritems().next()
    if trigger_set.is_all() == False:         return False
    if target_index == sm.init_state_index:   return False

    # The Acceptance State:
    #   -- has only one transition on 'all' to itself.
    #
    target_state = sm.states[target_index]
    if not target_state.is_acceptance():      return False
    tm = target_state.target_map.get_map()
    if len(tm) != 1:                          return False
    
    target_index_2, trigger_set = tm.iteritems().next()
    if trigger_set.is_all() == False:         return False
    if target_index_2 != target_index:        return False
    return True
    

