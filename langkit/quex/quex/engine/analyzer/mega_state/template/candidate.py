# vim:set encoding=utf8:
# (C) 2010-2012 Frank-Rene Schäfer
import quex.engine.analyzer.mega_state.template.gain_entry          as     gain_entry         
import quex.engine.analyzer.mega_state.template.gain_transition_map as     gain_transition_map


class TemplateStateCandidate(object):
    """________________________________________________________________________
    
    Contains information about a possible combination of two states into 
    a single template state. 
        
          .gain    = Integer that tells about the 'gain' of combining the 
                     concerned two states into a single one.

          .state_a, 
          .state_b = Reference to the two states under concern.
    
    The gain is computed from three components:
    
        entry gain: That is the gain which results from have the state
                    entries implemented in a single state. This gain 
                    results from command lists appearring in both states.
                    Instead of being implemented twice, they are 
                    implemented once.

        drop-out gain: Like 'entry gain' respectively for drop outs.

        transition map gain: The gain from combining the state's tran-
                             sition map.
    ___________________________________________________________________________
    """
    __slots__ = ("__gain", "__state_a", "__state_b")

    def __init__(self, StateA, StateB):

        # (*) Compute gain of combining the two states
        entry_gain          = gain_entry.do(StateA.entry, StateB.entry)

        transition_map_gain = gain_transition_map.do(StateA.transition_map,
                                                     len(StateA.implemented_state_index_set()),
                                                     StateA.target_scheme_n,
                                                     StateB.transition_map,
                                                     len(StateB.implemented_state_index_set()),
                                                     StateB.target_scheme_n)

        self.__gain = entry_gain + transition_map_gain

        # (*) Store reference to the two states
        self.__state_a = StateA
        self.__state_b = StateB

    @property 
    def gain(self):    return self.__gain
    @property
    def state_a(self): return self.__state_a
    @property
    def state_b(self): return self.__state_b

