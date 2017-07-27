# vim:set encoding=utf8:
# (C) 2010-2012 Frank-Rene Schäfer
from   quex.engine.analyzer.mega_state.target import TargetByStateKey
from   quex.engine.analyzer.transition_map    import TransitionMap       

from   itertools import chain

def do(ATm, AStateN, ASchemeN, BTm, BStateN, BSchemeN):
    """*Tm      -- transition map.
       *StateN  -- number of implemented states.
       *SchemeN -- number of different target schemes in transition map.
    
       Estimate the gain that can be achieved by combining two transition
       maps into a signle one.
    
    """
    # Costs of each single transition maps
    a_cost = __transition_map_cost(AStateN, len(ATm), ASchemeN)
    b_cost = __transition_map_cost(BStateN, len(BTm), BSchemeN)

    # Cost of the combined transition map
    combined_cost = _transition_cost_combined(ATm, BTm, AStateN + BStateN)

    return ((a_cost + b_cost) - combined_cost)
    
def _transition_cost_combined(TM_A, TM_B, ImplementedStateN):
    """Computes the storage consumption of a transition map.
    """
    # Count the number of unique schemes and the total interval number
    scheme_set       = set()
    uniform_target_n = 0
    interval_n       = 0
    for begin, end, a_target, b_target in TransitionMap.izip(TM_A, TM_B):
        interval_n += 1
        if     a_target.uniform_door_id is not None \
           and a_target.uniform_door_id == a_target.uniform_door_id:
            uniform_target_n += 1
        else:
            update_scheme_set(scheme_set, a_target, b_target)

    # The number of different schemes:
    scheme_n = len(scheme_set)

    return __transition_map_cost(ImplementedStateN, interval_n, scheme_n)

def __transition_map_cost(ImplementedStateN, IntervalN, SchemeN):
    """ImplementedStateN -- Number of states which are implemeted in the scheme.
       IntervalN         -- Number of intervals in the transition map.
       SchemeN           -- Number of DIFFERENT schemes in the transition map.
    
    Find a number which is proportional to the 'cost' of the transition
    map. Example:

         interval 1 --> [1, 3, 5, 1]
         interval 2 --> drop_out
         interval 3 --> [1, 3, 5, 1]
         interval 4 --> 5
         interval 5 --> [1, 3, 5, 1]
         interval 6 --> [2, 1, 1, 2]

     This transition map has 5 borders and 5 targets. Let the cost
     of a border as well as the cost for a single target be '1'.
     The additional cost for a required scheme is chosen to be 
     'number of scheme elements' which is the number of implemented
     states. Schemes that are the same are counted as 1.
    """
    #print "#ImplementedStateN", ImplementedStateN
    #print "#IntervalN", IntervalN
    #print "#SchemeN", SchemeN

    cost_border  = IntervalN - 1
    target_n     = IntervalN
    cost_targets = target_n + SchemeN * ImplementedStateN

    return cost_border + cost_targets

def update_scheme_set(scheme_set, TA, TB):
    """This function is used to count the number of different schemes in a
    combination of transition maps. The number of different schemes is used
    to determine the cost a combination of transition maps.

    NOTE: The use of 'hash' has the potential to miss a non-equal occurrence.
          The value is only for metrics. So its no great deal.

    RETURNS: True  -- if size remains the same
             False -- if size increases (scheme was new)
    """
    assert isinstance(TA, TargetByStateKey) 
    assert isinstance(TB, TargetByStateKey) 

    # The 'common drop_out case' is covered by 'uniform_door_id'
    if TA.uniform_door_id is not None:
        if TA.uniform_door_id == TB.uniform_door_id:
            return False

    my_hash = 0x5A5A5A5A
    prime   = 1299827  # Use a huge prime number for deterministic randomization
    for i, x in enumerate(chain(TA.iterable_door_id_scheme(), 
                                TB.iterable_door_id_scheme())):
        my_hash ^= hash(x) * i
        my_hash ^= prime

    size_before = len(scheme_set)
    scheme_set.add(my_hash)
    return size_before == len(scheme_set)
